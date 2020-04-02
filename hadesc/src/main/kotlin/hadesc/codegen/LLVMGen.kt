package hadesc.codegen

import dev.supergrecko.kllvm.core.types.IntType
import dev.supergrecko.kllvm.core.types.PointerType
import dev.supergrecko.kllvm.core.types.StructType
import dev.supergrecko.kllvm.core.values.ArrayValue
import dev.supergrecko.kllvm.core.values.FunctionValue
import dev.supergrecko.kllvm.core.values.InstructionValue
import dev.supergrecko.kllvm.core.values.IntValue
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.ValueBinding
import hadesc.types.Type
import llvm.*
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMTargetMachineRef
import org.bytedeco.llvm.global.LLVM

@OptIn(ExperimentalStdlibApi::class)
class LLVMGen(private val ctx: Context) : AutoCloseable {
    private val log = logger()
    private val llvmCtx = llvm.Context()
    private val llvmModule = llvm.Module(ctx.options.main.toString(), llvmCtx)
    private val builder = llvm.Builder(llvmCtx)
    private val qualifiedNameToFunctionValue = mutableMapOf<QualifiedName, FunctionValue>()
    private val specializationStacks = mutableMapOf<QualifiedName, MutableList<FunctionValue>>()

    fun generate() {
        log.info("Generating LLVM IR")
        lowerSourceFile(ctx.sourceFile(QualifiedName(), ctx.mainPath()))
        log.debug(LLVM.LLVMPrintModuleToString(llvmModule.getUnderlyingReference()).string)
        verifyModule()
        writeModuleToFile()
        linkWithRuntime()
    }

    private fun verifyModule() {
        // TODO: Handle this in a better way
        val buffer = ByteArray(100)
        val len = LLVM.LLVMVerifyModule(llvmModule.getUnderlyingReference(), 100, buffer)
        println(buffer.decodeToString().slice(0 until len))
    }

    private val completedSourceFiles = mutableSetOf<SourcePath>()
    private fun lowerSourceFile(sourceFile: SourceFile) {
        if (completedSourceFiles.contains(sourceFile.location.file)) {
            return
        }
        log.debug("START: LLVMGen::lowerSourceFile(${sourceFile.location.file})")
        for (declaration in sourceFile.declarations) {
            ctx.checker.checkDeclaration(declaration)
            lowerDeclaration(declaration)
        }
        completedSourceFiles.add(sourceFile.location.file)
        log.debug("DONE: LLVMGen::lowerSourceFile(${sourceFile.location.file})")
    }

    private fun lowerDeclaration(declaration: Declaration) = when (declaration) {
        is Declaration.ImportAs -> {
            // Imports don't generate anything
        }
        is Declaration.FunctionDef -> lowerFunctionDefDeclaration(declaration)
        is Declaration.Error -> {
        }
        is Declaration.ExternFunctionDef -> lowerExternDefDeclaration(declaration)
        is Declaration.Struct -> {
            lowerStructDeclaration(declaration)
        }
    }

    private fun lowerStructDeclaration(decl: Declaration.Struct) {
        val constructorType = ctx.checker.typeOfStructConstructor(decl)
        val instanceType = ctx.checker.typeOfStructInstance(decl)
        val constructorFunction = llvmModule.addFunction(
            lowerBinder(decl.binder),
            lowerType(constructorType) as FunctionType
        )
        val basicBlock = constructorFunction.appendBasicBlock("entry")

        generateInBlock(basicBlock) {

            val thisPtr = buildAlloca(
                lowerType(instanceType),
                "instance"
            )

            val instanceStructType = instanceType as Type.Struct
            for (i in instanceStructType.memberTypes.entries.indices) {
                val paramRef = constructorFunction.getParam(i)
                val elementPtr = buildStructGEP(
                    thisPtr,
                    i,
                    "field_${i}"
                )
                buildStore(paramRef, elementPtr)

            }

            val result = buildLoad(thisPtr, "result")
            buildRet(result)

        }
        constructorFunction.verify()
    }

    private fun FunctionValue.verify() {
        val validate = LLVM.LLVMVerifyFunction(getUnderlyingReference(), LLVM.LLVMPrintMessageAction)
        if (validate > 0) {
            log.debug("Bad function: ${dumpToString()}")
            TODO()
        }
    }

    private fun generateInBlock(basicBlock: BasicBlock, function: Builder.() -> Unit) {
        val oldPosition = builder.getInsertBlock()
        builder.positionAtEnd(basicBlock)

        builder.function()

        if (oldPosition != null) {
            builder.positionAtEnd(oldPosition)
        }
    }

    private fun lowerExternDefDeclaration(
        decl: Declaration.ExternFunctionDef
    ) {
        llvmModule.addFunction(
            decl.externName.name.text, FunctionType(
                returns = lowerTypeAnnotation(decl.returnType),
                types = decl.paramTypes.map { lowerTypeAnnotation(it) },
                variadic = false
            )
        )
    }

    private fun lowerFunctionDefDeclaration(def: Declaration.FunctionDef) {
        if (def.typeParams.isNotEmpty()) {
            // we can't lower generic functions here.
            // we have to generate seperate versions
            // of this function for each call site with
            // type parameters substituted with actual
            // types
            return
        }
        val binding = ctx.resolver.getBinding(def.name.identifier)
        val qualifiedName = when (binding) {
            is ValueBinding.GlobalFunction -> binding.qualifiedName
            else -> {
                throw AssertionError(
                    "Expected function def declaration to bind to ValueBinding.GlobalFunction"
                )
            }
        }

        val returnType = ctx.checker.annotationToType(def.returnType)
        val type = FunctionType(
            lowerTypeAnnotation(def.returnType),
            def.params.map { lowerParamToType(it) },
            false
        )
        val func = llvmModule.addFunction(lowerBinder(def.name), type)

        qualifiedNameToFunctionValue[qualifiedName] = func
        LLVM.LLVMSetLinkage(func.getUnderlyingReference(), LLVM.LLVMExternalLinkage)
        val basicBlock = func.appendBasicBlock("entry")
        generateInBlock(basicBlock) {
            for (member in def.body.members) {
                lowerBlockMember(member)
            }
            if (returnType == Type.Void) {
                builder.buildRetVoid()
            }
        }

        log.debug("function: $qualifiedName; params: ${def.params.size}")
        log.debug(func.dumpToString())
        func.verify()
    }

    private fun lowerTypeAnnotation(annotation: TypeAnnotation): llvm.Type {
        return lowerType(ctx.checker.annotationToType(annotation))
    }

    private fun lowerType(type: Type): llvm.Type = when (type) {
        Type.Error -> TODO()
        Type.Byte -> byteTy
        Type.Void -> voidTy
        is Type.ModuleAlias -> TODO("Bug: Module alias can't be lowered")
        is Type.Bool -> boolTy
        is Type.RawPtr -> ptrTy(lowerType(type.to))
        is Type.Function -> FunctionType(
            returns = lowerType(type.to),
            types = type.from.map { lowerType(it) },
            variadic = false
        )
        is Type.Struct -> StructType(
            type.memberTypes.values.map { lowerType(it) },
            packed = false,
            ctx = llvmCtx
        )
        is Type.ParamRef, is Type.GenericFunction, is Type.Deferred ->
            TODO("Can't lower unspecialized type param")
    }

    private fun lowerParamToType(param: Param): llvm.Type {
        assert(param.annotation != null) { "Inferred param types not implemented yet" }
        return lowerTypeAnnotation(param.annotation as TypeAnnotation)
    }

    private fun lowerBlockMember(member: Block.Member): Unit = when (member) {
        is Block.Member.Expression -> {
            lowerExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> {
            lowerStatement(member.statement)
        }
    }

    private fun lowerStatement(statement: Statement): Unit = when (statement) {
        is Statement.Return -> lowerReturnStatement(statement)
        is Statement.Val -> lowerValStatement(statement)
        is Statement.Error -> TODO()
    }

    private val localVariables = mutableMapOf<SourceLocation, llvm.Value>()
    private fun lowerValStatement(kind: Statement.Val) {
        val instr = builder.buildAlloca(
            lowerType(ctx.checker.typeOfExpression(kind.rhs)),
            kind.binder.identifier.name.text + "_tmp"
        )
        val rhs = lowerExpression(kind.rhs)
        builder.buildStore(rhs, instr)
        localVariables[kind.binder.location] = instr
        log.debug(LLVM.LLVMPrintModuleToString(llvmModule.getUnderlyingReference()).string)
    }

    private fun lowerReturnStatement(statement: Statement.Return) {
        builder.buildRet(lowerExpression(statement.value))
    }

    private fun lowerExpression(expr: Expression): llvm.Value {
        // for now, this is only to ensure that we check each expression
        // eventually, the pipeline should ensure that everything is
        // typechecked before we reach codegen phase
        ctx.checker.typeOfExpression(expr)
        return when (expr) {
            is Expression.Error -> TODO("Syntax error: ${expr.location}")
            is Expression.Var -> lowerVarExpression(expr)
            is Expression.Call -> {
                lowerCallExpression(expr)
            }
            is Expression.Property -> lowerPropertyExpression(expr)
            is Expression.ByteString -> lowerByteStringExpression(expr)
            is Expression.BoolLiteral -> {
                if (expr.value) {
                    trueValue
                } else {
                    falseValue
                }
            }
        }
    }

    private fun lowerPropertyExpression(expr: Expression.Property): Value {
        // the reason this isn't as simple as lowerLHS -> extractvalue on lowered lhs
        // is because lhs might not be an actual runtime value, for example when
        // it refers to a module alias (import x as y). Here, y isn't a real value
        // in that case, we have to resolve y.z to a qualified name and use that
        // value instead (x.y.z).
        // If it's not a module alias, then we do the extractvalue (lower lhs) thingy
        // (lowerValuePropertyExpression)
        return when (expr.lhs) {
            is Expression.Var -> {
                when (val binding = ctx.resolver.getBinding(expr.lhs.name)) {
                    is ValueBinding.ImportAs -> {
                        val sourceFile = ctx.resolveSourceFile(binding.declaration.modulePath)
                        lowerSourceFile(sourceFile)
                        val qualifiedName = ctx.resolver.findInSourceFile(expr.property, sourceFile)?.qualifiedName
                            ?: throw AssertionError("${expr.location}: No such property")
                        lowerQualifiedValueName(qualifiedName)
                    }
                    else -> {
                        lowerValuePropertyExpression(expr)
                    }
                }
            }
            else ->
                lowerValuePropertyExpression(expr)
        }
    }

    private fun lowerValuePropertyExpression(
        expression: Expression.Property
    ): Value {
        val lhsPtr = lowerExpression(expression.lhs)
        return when (val type = ctx.checker.typeOfExpression(expression.lhs)) {
            Type.Byte,
            Type.Void,
            Type.Error,
            Type.Bool,
            is Type.RawPtr,
            is Type.Function -> TODO("Can't call dot operator")
            is Type.Struct -> {
                val rhsName = expression.property.name
                val index = type.memberTypes.keys.indexOf(rhsName)
                builder.buildExtractValue(
                    lhsPtr,
                    index,
                    ""
                )
            }
            is Type.ModuleAlias -> TODO("Should not be reached")
            is Type.ParamRef, is Type.GenericFunction -> TODO("Can't lower unspecialized type")
            is Type.Deferred -> TODO("Can't lower unspecialized type")
        }
    }

    private fun lowerByteStringExpression(expression: Expression.ByteString): Value {
        val text = expression.bytes.decodeToString()
        val constStringRef = ArrayValue(text, nullTerminate = false, context = llvmCtx)
        val globalRef = llvmModule.addGlobal(
            constStringRef.getType(),
            stringLiteralName()
        )
        globalRef.initializer = constStringRef
        return globalRef.constPointerCast(bytePtrTy)
    }

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "\$string_literal_$nextLiteralIndex"
    }

    private var nextNameIndex = 0
    private fun generateUniqueName(): String {
        nextNameIndex++
        return "\$_$nextNameIndex"
    }

    private val byteTy = IntType(8, llvmCtx)
    private val bytePtrTy = PointerType(byteTy)
    private val voidTy = VoidType(llvmCtx)
    private val boolTy = IntType(1, llvmCtx)
    private val trueValue = IntValue(boolTy, 1, false)
    private val falseValue = IntValue(boolTy, 0, false)

    private fun ptrTy(to: llvm.Type): llvm.Type {
        return PointerType(to)
    }

    private fun lowerQualifiedValueName(qualifiedName: QualifiedName): Value {
        val binding = ctx.resolver.resolveQualifiedName(qualifiedName)
        return when (binding) {
            null -> TODO("Unbound name $qualifiedName")
            is ValueBinding.GlobalFunction -> {
                llvmModule.getFunction(mangleQualifiedName(binding.qualifiedName))
                    ?: throw AssertionError(
                        "Function ${binding.qualifiedName} hasn't been added to llvm module"
                    )
            }
            is ValueBinding.ExternFunction ->
                llvmModule.getFunction(binding.declaration.externName.name.text)
                    ?: throw AssertionError(
                        "Function ${binding.qualifiedName} hasn't been added to llvm module"
                    )

            is ValueBinding.FunctionParam -> {
                assert(qualifiedName.size == 1)
                val name = qualifiedName.first
                val functionQualifiedName = ctx.resolver.getBinding(binding.declaration.name.identifier).qualifiedName
                val index = binding.declaration.params.indexOfFirst { it.binder.identifier.name == name }
                assert(index > -1)
                val specialization = specializationStacks[functionQualifiedName]?.lastOrNull()
                if (specialization != null) {
                    specialization.getParam(index)
                } else {
                    val functionValue: FunctionValue = qualifiedNameToFunctionValue[functionQualifiedName]
                        ?: throw AssertionError("no function value for param binding")
                    functionValue.getParam(index)
                }
            }
            is ValueBinding.ImportAs -> TODO("${binding.declaration.asName.identifier.name.text} is not a valid expression")
            is ValueBinding.ValBinding ->
                builder.buildLoad(
                    localVariables[binding.statement.binder.location] ?: TODO("Unbound"),
                    generateUniqueName()
                )
            is ValueBinding.Struct ->
                llvmModule.getFunction(mangleQualifiedName(binding.qualifiedName))
                    ?: throw AssertionError(
                        "Function ${binding.qualifiedName} hasn't been added to llvm module"
                    )
        }
    }

    private fun lowerVarExpression(kind: Expression.Var): Value {
        val binding = ctx.resolver.getBinding(kind.name)
        return lowerQualifiedValueName(binding.qualifiedName)
    }

    private fun mangleQualifiedName(qualifiedName: QualifiedName): String {
        return qualifiedName.mangle()
    }


    private fun lowerCallExpression(
        call: Expression.Call
    ): InstructionValue {
        if (ctx.checker.isGenericCallSite(call)) {
            return generateGenericCall(call)
        }
        return builder.buildCall(
            lowerExpression(call.callee),
            call.args.map { lowerExpression(it.expression) }
        )
    }

    private fun generateGenericCall(call: Expression.Call): InstructionValue {
        val def = getFunctionDef(call.callee)
        val specializedFunctionType = ctx.checker
            .getGenericSpecializedFunctionType(def, call.location, call.args)
        val originalFunctionName =
            ctx.resolver.getBinding(def.name.identifier).qualifiedName
        val specializedFunctionName = originalFunctionName.append(ctx.makeName(call.location.toString()))
        val fn = llvmModule.addFunction(
            mangleQualifiedName(specializedFunctionName),
            lowerType(specializedFunctionType) as FunctionType
        )
        val stack = specializationStacks
            .computeIfAbsent(originalFunctionName) { mutableListOf() }
        stack.add(fn)
        generateSpecializedFunctionBody(fn, specializedFunctionType, def)
        stack.removeLast()
        return builder.buildCall(
            fn,
            call.args.map { lowerExpression(it.expression) }
        )
    }

    private fun generateSpecializedFunctionBody(
        fn: FunctionValue,
        specializedFunctionType: Type.Function,
        def: Declaration.FunctionDef
    ) {
        val basicBlock = fn.appendBasicBlock("entry")
        generateInBlock(basicBlock) {
            def.body.members.forEach {
                lowerBlockMember(it)
            }
        }

    }

    private fun getFunctionDef(callee: Expression): Declaration.FunctionDef {
        return when (callee) {
            is Expression.Var -> resolveGlobalFunctionDefFromVariable(callee.name)
            is Expression.Property -> resolveGlobalFunctionDefForModule(callee.lhs, callee.property)
            else -> TODO("${callee.location}: Can't lower generic call for non global functions right now")
        }
    }

    private fun resolveGlobalFunctionDefFromVariable(name: Identifier): Declaration.FunctionDef {
        return when (val binding = ctx.resolver.getBinding(name)) {
            is ValueBinding.GlobalFunction -> binding.declaration
            else -> TODO("${name.location}: Not a function definition")
        }
    }

    private fun resolveGlobalFunctionDefForModule(lhs: Expression, property: Identifier): Declaration.FunctionDef {
        TODO("Generic functions not implemented across modules")
    }


    private fun lowerBinder(binder: Binder): String {
        return if (binder.identifier.name.text == "main") {
            "hades_main"
        } else {
            mangleQualifiedName(ctx.resolver.getBinding(binder.identifier).qualifiedName)
        }
    }

    private fun linkWithRuntime() {
        log.info("Linking using gcc")
        val commandParts = listOf(
            "gcc",
            "-no-pie",
            "-o", ctx.options.output.toString(),
            ctx.options.runtime.toString(),
            objectFilePath
        )
        log.info(commandParts.joinToString(" "))
        val builder = ProcessBuilder(commandParts).inheritIO()
        log.debug(builder.command().joinToString(","))
        val process = builder.start()
        val exitCode = process.waitFor()
        assert(exitCode == 0) {
            "gcc exited with code $exitCode"
        }
    }

    private val objectFilePath get() = ctx.options.output.toString() + ".object"

    private fun writeModuleToFile() {
        log.info("Writing object file")
        LLVM.LLVMInitializeAllTargetInfos()
        LLVM.LLVMInitializeAllTargets()
        LLVM.LLVMInitializeAllTargetMCs()
        LLVM.LLVMInitializeAllAsmParsers()
        LLVM.LLVMInitializeAllAsmPrinters()
        val targetTriple = LLVM.LLVMGetDefaultTargetTriple()
        val cpu = BytePointer("generic")
        val features = BytePointer("")
        val target = LLVM.LLVMGetFirstTarget()
        val targetMachine: LLVMTargetMachineRef = LLVM.LLVMCreateTargetMachine(
            target,
            targetTriple,
            cpu,
            features,
            LLVM.LLVMCodeGenLevelLess,
            LLVM.LLVMRelocDefault,
            LLVM.LLVMCodeModelDefault
        )

        val pass = LLVM.LLVMCreatePassManager()
        LLVM.LLVMTargetMachineEmitToFile(
            targetMachine,
            llvmModule.getUnderlyingReference(),
            BytePointer(objectFilePath),
            LLVM.LLVMObjectFile,
            BytePointer("Message")
        )
        LLVM.LLVMRunPassManager(pass, llvmModule.getUnderlyingReference())

        LLVM.LLVMDisposePassManager(pass)
        LLVM.LLVMDisposeTargetMachine(targetMachine)
    }

    override fun close() {
        llvmCtx.dispose()
    }
}
