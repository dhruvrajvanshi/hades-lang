package hadesc.codegen

import dev.supergrecko.kllvm.core.types.IntType
import dev.supergrecko.kllvm.core.types.PointerType
import dev.supergrecko.kllvm.core.values.FunctionValue
import dev.supergrecko.kllvm.core.values.InstructionValue
import dev.supergrecko.kllvm.core.values.PointerValue
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.ValueBinding
import hadesc.types.Type
import llvm.FunctionType
import llvm.Value
import llvm.VoidType
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMTargetMachineRef
import org.bytedeco.llvm.global.LLVM

@OptIn(ExperimentalStdlibApi::class)
class LLVMGen(private val ctx: Context) : AutoCloseable {
    private val log = logger()
    private val llvmCtx = llvm.Context.create()
    private val llvmModule = llvm.Module.create(ctx.options.main.toString(), llvmCtx)
    private val builder = llvm.Builder.create(llvmCtx)
    private val qualifiedNameToFunctionValue = mutableMapOf<QualifiedName, FunctionValue>()

    fun generate() {
        log.info("Generating LLVM IR")
        lowerSourceFile(ctx.sourceFile(QualifiedName(), ctx.mainPath()))
        llvmModule.dump()
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
            lowerDeclaration(declaration)
        }
        completedSourceFiles.add(sourceFile.location.file)
        log.debug("DONE: LLVMGen::lowerSourceFile(${sourceFile.location.file})")
    }

    private fun lowerDeclaration(declaration: Declaration) = when (declaration.kind) {
        is Declaration.Kind.ImportAs -> {
            // Imports don't generate anything
        }
        is Declaration.Kind.FunctionDef -> lowerFunctionDefDeclaration(declaration, declaration.kind)
        Declaration.Kind.Error -> {
        }
        is Declaration.Kind.ExternFunctionDef -> lowerExternDefDeclaration(declaration, declaration.kind)
    }

    private fun lowerExternDefDeclaration(declaration: Declaration, kind: Declaration.Kind.ExternFunctionDef) {
        llvmModule.addFunction(
            kind.externName.name.text, FunctionType.new(
                returns = VoidType.new(llvmCtx),
                types = listOf(PointerType.new(IntType.new(8, llvmCtx))),
                variadic = false
            )
        )
    }

    private fun lowerFunctionDefDeclaration(declaration: Declaration, def: Declaration.Kind.FunctionDef) {
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
        val type = FunctionType.new(
            lowerTypeAnnotation(def.returnType),
            def.params.map { lowerParamToType(it) },
            false
        )
        val func = llvmModule.addFunction(lowerBinder(def.name), type)

        qualifiedNameToFunctionValue[qualifiedName] = func

        LLVM.LLVMSetLinkage(func.getUnderlyingReference(), LLVM.LLVMExternalLinkage)
        val basicBlock = func.appendBasicBlock("entry")
        val previousPosition = builder.getInsertBlock()
        builder.positionAtEnd(basicBlock)
        for (member in def.body.members) {
            lowerBlockMember(member)
        }
        if (returnType == Type.Void) {
            builder.buildRetVoid()
        }
        log.debug("function: $qualifiedName; params: ${def.params.size}")
        log.debug(func.dumpToString())
        if (previousPosition != null) {
            builder.positionAtEnd(previousPosition)
        }
        LLVM.LLVMVerifyFunction(func.getUnderlyingReference(), LLVM.LLVMAbortProcessAction)
    }

    private fun lowerTypeAnnotation(annotation: TypeAnnotation): llvm.Type {
        return lowerType(ctx.checker.annotationToType(annotation))
    }

    private fun lowerType(type: Type): llvm.Type = when (type) {
        Type.Byte -> byteTy
        Type.Void -> voidTy
        is Type.RawPtr -> ptrTy(lowerType(type.to))
        is Type.Function -> FunctionType.new(
            returns = lowerType(type.to),
            types = type.from.map { lowerType(it) },
            variadic = false
        )
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

    private fun lowerStatement(statement: Statement): Unit = when (statement.kind) {
        is Statement.Kind.Return -> lowerReturnStatement(statement, statement.kind)
        Statement.Kind.Error -> TODO()
    }

    private fun lowerReturnStatement(statement: Statement, kind: Statement.Kind.Return) {
        builder.buildRet(lowerExpression(kind.value))
    }

    private fun lowerExpression(expr: Expression): llvm.Value = when (expr.kind) {
        Expression.Kind.Error -> TODO("Syntax error: ${expr.location}")
        is Expression.Kind.Var -> lowerVarExpression(expr, expr.kind)
        is Expression.Kind.Call -> {
            lowerCallExpression(expr, expr.kind)
        }
        is Expression.Kind.Property -> lowerPropertyExpression(expr, expr.kind)
        is Expression.Kind.ByteString -> lowerByteStringExpression(expr, expr.kind)
    }

    private fun lowerPropertyExpression(expr: Expression, kind: Expression.Kind.Property): Value {
        return when (kind.lhs.kind) {
            is Expression.Kind.Var -> {
                val binding = ctx.resolver.getBinding(kind.lhs.kind.name)
                when (binding) {
                    is ValueBinding.ImportAs -> {
                        val sourceFile = ctx.resolveSourceFile(binding.kind.modulePath)
                        lowerSourceFile(sourceFile)
                        val qualifiedName = ctx.resolver.findInSourceFile(kind.property, sourceFile)?.qualifiedName
                            ?: throw AssertionError("${expr.location}: No such property")
                        lowerQualifiedValueName(qualifiedName)
                    }
                    else -> TODO("${expr.location}: Can't select property on this expression")
                }
            }
            else -> TODO("${expr.location}: Can't select property on this expression")

        }
    }

    private fun lowerByteStringExpression(expr: Expression, kind: Expression.Kind.ByteString): Value {
        val text = kind.bytes.decodeToString()
        val constStringRef = LLVM.LLVMConstString(text, text.length, 0)
        val globalRef = LLVM.LLVMAddGlobal(
            llvmModule.getUnderlyingReference(),
            LLVM.LLVMTypeOf(constStringRef),
            stringLiteralName()
        )
        LLVM.LLVMSetInitializer(globalRef, constStringRef)
        val ptrRef = LLVM.LLVMConstPointerCast(globalRef, bytePtrTy.getUnderlyingReference())
        return PointerValue(ptrRef)
    }

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "\$string_literal_$nextLiteralIndex"
    }

    private val byteTy = IntType.new(8, llvmCtx)
    private val bytePtrTy = PointerType.new(byteTy)
    private val voidTy = VoidType.new(llvmCtx)

    private fun ptrTy(to: llvm.Type): llvm.Type {
        return PointerType.new(to)
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
                llvmModule.getFunction(binding.kind.binder.identifier.name.text)
                    ?: throw AssertionError(
                        "Function ${binding.qualifiedName} hasn't been added to llvm module"
                    )

            is ValueBinding.FunctionParam -> {
                assert(qualifiedName.size == 1)
                val name = qualifiedName.first
                val functionQualifiedName = ctx.resolver.getBinding(binding.kind.name.identifier).qualifiedName
                val functionValue: FunctionValue = qualifiedNameToFunctionValue[functionQualifiedName]
                    ?: throw AssertionError("no function value for param binding")
                val index = binding.kind.params.indexOfFirst { it.binder.identifier.name == name }
                assert(index > -1)
                FunctionValue(LLVM.LLVMGetParam(functionValue.getUnderlyingReference(), index))
            }
            is ValueBinding.ImportAs -> TODO("${binding.kind.asName.identifier.name.text} is not a valid expression")
        }
    }

    private fun lowerVarExpression(expr: Expression, kind: Expression.Kind.Var): Value {
        val binding = ctx.resolver.getBinding(kind.name)
        return lowerQualifiedValueName(binding.qualifiedName)
    }

    private fun mangleQualifiedName(qualifiedName: QualifiedName): String {
        return qualifiedName.mangle()
    }


    private fun lowerCallExpression(
        expr: Expression,
        kind: Expression.Kind.Call
    ): InstructionValue {
        return builder.buildCall(
            lowerExpression(kind.callee),
            kind.args.map { lowerExpression(it.expression) }
        )
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
        val builder = ProcessBuilder(
            "gcc",
            "-no-pie",
            "-o", ctx.options.output.toString(),
            "runtime.c",
            objectFilePath
        ).inheritIO()
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