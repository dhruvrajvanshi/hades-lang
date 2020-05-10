package hadesc.codegen

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.ir.*
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import llvm.*
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMTargetMachineRef
import org.bytedeco.llvm.global.LLVM
import java.nio.charset.StandardCharsets

@OptIn(ExperimentalStdlibApi::class)
class LLVMGen(private val ctx: Context, private val irModule: IRModule) : AutoCloseable {
    private var currentFunction: FunctionValue? = null
    private val log = logger()
    private val llvmCtx = llvm.Context()
    private val llvmModule = llvm.Module(ctx.options.main.toString(), llvmCtx)
    private val builder = llvm.Builder(llvmCtx)

    fun generate() {
        for (it in irModule) {
            lowerDefinition(it)
        }
        verifyModule()
        writeModuleToFile()
        linkWithRuntime()
    }

    private fun lowerDefinition(definition: IRDefinition): Unit =
        when (definition) {
            is IRFunctionDef -> lowerFunctionDef(definition)
            is IRStructDef -> lowerStructDef(definition)
            is IRExternFunctionDef -> lowerExternFunctionDef(definition)
            is IRConstDef -> lowerConstDef(definition)
        }

    private val loweredGlobals = mutableMapOf<IRGlobalName, Value>()
    private fun lowerConstDef(definition: IRConstDef) {
        val name = lowerName(definition.name)
        if (loweredGlobals[definition.name] == null) {
            val global = llvmModule.addGlobal(name, lowerType(definition.type))
            global.initializer = lowerExpression(definition.initializer)
            loweredGlobals[definition.name] = global.initializer
        }
    }

    private fun getConstDefValue(def: IRConstDef): llvm.Value  {
        if (loweredGlobals[def.name] == null) {
            lowerConstDef(def)
        }
        return requireNotNull(loweredGlobals[def.name])
    }

    private fun lowerStructDef(definition: IRStructDef) {
        val fn = getStructConstructor(definition)
        withinBlock(fn.appendBasicBlock("entry")) {
            val instanceType = lowerType(definition.instanceType)
            val thisPtr = buildAlloca(instanceType, "this")
            var index = -1
            for (field in definition.fields) {
                index++
                val fieldPtr = buildStructGEP(thisPtr, index, "field_$index")
                val value = fn.getParameter(index)
                buildStore(toPointer = fieldPtr, value = value)
            }
            val instance = buildLoad(thisPtr, "instance")
            buildRet(instance)
        }

    }

    private fun lowerExternFunctionDef(definition: IRExternFunctionDef) {
        getDeclaration(definition)
    }

    private fun lowerFunctionDef(definition: IRFunctionDef) {
        val fn = getDeclaration(definition)
        currentFunction = fn
        definition.params.forEachIndexed { index, param ->
            localVariables[param.name] = fn.getParameter(index)
        }
        lowerBlock(definition.entryBlock)
        for (block in definition.blocks) {
            lowerBlock(block)
        }
    }

    private val blocks = mutableMapOf<Pair<String, IRLocalName>, BasicBlock>()
    private fun getBlock(blockName: IRLocalName): BasicBlock {
        val fnName = requireNotNull(currentFunction?.valueName)
        return blocks.computeIfAbsent(fnName to blockName) {
            requireNotNull(currentFunction).appendBasicBlock(it.second.mangle())
        }
    }

    private val loweredBlocks = mutableMapOf<IRBlock, BasicBlock>()
    private fun lowerBlock(block: IRBlock): BasicBlock = loweredBlocks.computeIfAbsent(block) {
        val basicBlock = getBlock(block.name)
        builder.positionAtEnd(basicBlock)
        for (statement in block) {
            lowerStatement(statement)
        }
        basicBlock
    }

    private fun lowerStatement(statement: IRStatement) = when (statement) {
        is IRReturnStatement -> lowerReturnStatement(statement)
        is IRReturnVoidStatement -> {
            builder.buildRetVoid()
            Unit
        }
        is IRValue -> {
            lowerExpression(statement)
            Unit
        }
        is IRCall -> {
            lowerCallExpression(statement)
            Unit
        }
        is IRAlloca -> lowerAlloca(statement)
        is IRStore -> lowerStore(statement)
        is IRLoad -> lowerLoad(statement)
        is IRNot -> lowerNot(statement)
        is IRBr -> lowerBr(statement)
        is IRJump -> lowerJump(statement)
    }

    private fun lowerJump(statement: IRJump) {
        LLVM.LLVMBuildBr(builder.ref, getBlock(statement.label).ref)
    }

    private fun lowerBr(statement: IRBr) {
        builder.buildCondBr(lowerExpression(
            statement.condition),
            getBlock(statement.ifTrue),
            getBlock(statement.ifFalse))
    }

    private fun lowerNot(statement: IRNot) {
        val name = lowerName(statement.name)
        localVariables[statement.name] = builder.buildNot(lowerExpression(statement.arg), name)
    }

    private val localVariables = mutableMapOf<IRLocalName, llvm.Value>()

    private fun lowerAlloca(statement: IRAlloca) {
        val name = lowerName(statement.name)
        val ref = builder.buildAlloca(lowerType(statement.type), name)
        localVariables[statement.name] = ref
    }

    private fun lowerStore(statement: IRStore) {
        builder.buildStore(
            value = lowerExpression(statement.value),
            toPointer = lowerExpression(statement.ptr)
        )
    }

    private fun lowerLoad(statement: IRLoad) {
        val ref = builder.buildLoad(
            ptr = lowerExpression(statement.ptr),
            name = lowerName(statement.name)
        )
        localVariables[statement.name] = ref
    }

    private fun getLocalVariable(name: IRLocalName): llvm.Value {
        return requireNotNull(localVariables[name])
    }


    private fun lowerReturnStatement(statement: IRReturnStatement) {
        builder.buildRet(lowerExpression(statement.value))
    }

    private fun lowerExpression(value: IRValue): llvm.Value = when (value) {
        is IRBool -> lowerBoolExpression(value)
        is IRByteString -> lowerByteString(value)
        is IRVariable -> lowerVariable(value)
        is IRGetStructField -> lowerGetStructField(value)
        is IRCIntConstant -> lowerCIntValue(value)
        is IRNullPtr -> lowerNullPtr(value)
        is IRMethodRef -> requireUnreachable()
    }

    private fun lowerCIntValue(value: IRCIntConstant): llvm.Value {
        return ConstantInt(value = value.value.toLong(), type = cIntTy, signExtend = false)
    }

    private fun lowerNullPtr(value: IRNullPtr): llvm.Value {
        return (lowerType(value.type) as PointerType).getConstantNullPointer()
    }

    private fun lowerGetStructField(expression: IRGetStructField): llvm.Value {
        return builder.buildExtractValue(
            lowerExpression(expression.lhs),
            expression.index,
            ctx.makeUniqueName().text
        )
    }

    private fun lowerBoolExpression(expression: IRBool): llvm.Value {
        return if (expression.value) {
            trueValue
        } else {
            falseValue
        }
    }

    private fun lowerVariable(expression: IRVariable): llvm.Value {
        return when (expression.name) {
            is IRLocalName -> requireNotNull(localVariables[expression.name]) {
                "Unbound variable: ${expression.name.prettyPrint()}"
            }
            is IRGlobalName -> lowerGlobalVariable(expression.name)
        }
    }


    private fun lowerGlobalVariable(name: IRGlobalName) = when (val binding = irModule.resolveGlobal(name)) {
        is IRBinding.FunctionDef -> getDeclaration(binding.def)
        is IRBinding.ExternFunctionDef -> getDeclaration(binding.def)
        is IRBinding.StructDef -> getStructConstructor(binding.def)
        is IRBinding.ConstDef -> getConstDefValue(binding.def)
    }

    private fun lowerByteString(expression: IRByteString): llvm.Value {
        val text = expression.value.decodeToString()
        val constStringRef = llvm.ConstantArray(text, nullTerminate = false, context = llvmCtx)
        val globalRef = llvmModule.addGlobal(
            stringLiteralName(),
            constStringRef.getType()
        )
        globalRef.initializer = constStringRef
        return llvm.Value(LLVM.LLVMConstPointerCast(globalRef.ref, bytePtrTy.ref))
    }

    private fun lowerCallExpression(expression: IRCall): llvm.Value {
        val callee = lowerExpression(expression.callee)
        require(expression.typeArgs == null) { "Unspecialized generic function found in LLVMGen" }
        val args = expression.args.map { lowerExpression(it) }
        val ref = builder.buildCall(callee, args, if (expression.type == Type.Void) null else expression.name.mangle())

        localVariables[expression.name] = ref

        return ref
    }

    private fun withinBlock(basicBlock: llvm.BasicBlock, fn: llvm.Builder.() -> Unit) {
        builder.positionAtEnd(basicBlock)
        builder.fn()
    }


    private fun getDeclaration(externFunctionDef: IRExternFunctionDef): llvm.FunctionValue {
        val irName = externFunctionDef.externName
        val type = lowerFunctionType(externFunctionDef.type)
        val name = if (irName.text == "main") {
            "hades_main"
        } else {
            irName.text
        }
        return llvmModule.getFunction(name)?.asFunctionValue() ?: llvmModule.addFunction(name, type)
    }

    private fun getDeclaration(def: IRFunctionDef): llvm.FunctionValue {
        val irName = def.name
        val type = lowerFunctionType(def.type)
        val name = if (irName.mangle() == "main") {
            "hades_main"
        } else {
            lowerName(irName)
        }
        return (llvmModule.getFunction(name) ?: llvmModule.addFunction(name, type)).asFunctionValue()
    }

    private fun getStructConstructor(def: IRStructDef): llvm.FunctionValue {
        val name = lowerName(def.globalName)
        val existing = llvmModule.getFunction(name)?.asFunctionValue()
        return if (existing != null) {
            existing
        } else {
            val constructorType = def.constructorType
            require(constructorType is Type.Function)
            val loweredType = lowerFunctionType(constructorType)
            llvmModule.addFunction(name, loweredType)
        }


    }


    private fun lowerFunctionType(type: Type): llvm.FunctionType {
        val lowered = lowerType(type)
        require(lowered is llvm.FunctionType)
        return lowered
    }

    private fun lowerName(name: IRName): String {
        return name.mangle()
    }

    private fun verifyModule() {
        // TODO: Handle this in a better way
        val buffer = ByteArray(100)
        val error = LLVM.LLVMVerifyModule(llvmModule.ref, LLVM.LLVMPrintMessageAction, buffer)
        require(error == 0) {
            log.error("Invalid llvm module: ${llvmModule.sourceFileName}\n")
            "Invalid LLVM module"
        }
    }

    private fun llvm.FunctionValue.verify() {
        val validate = LLVM.LLVMVerifyFunction(ref, LLVM.LLVMPrintMessageAction)
        if (validate > 0) {
            log.debug("Bad function: ${this.dumpToString()}")
            TODO()
        }
    }

    private val structTypes = mutableMapOf<QualifiedName, StructType>()
    private fun lowerType(type: Type): llvm.Type = when (type) {
        Type.Error -> requireUnreachable()
        Type.Byte -> byteTy
        Type.Void -> voidTy
        is Type.Bool -> boolTy
        Type.CInt -> cIntTy
        is Type.RawPtr -> ptrTy(lowerType(type.to))
        is Type.Function -> {
            require(type.typeParams == null) {
                "Can't lower unspecialized generic function type"
            }
            FunctionType(
                returns = lowerType(type.to),
                types = type.from.map { lowerType(it) },
                variadic = false
            )
        }
        is Type.Struct -> structTypes.computeIfAbsent(type.constructor.name) {
            val name = type.constructor.name.mangle()
            val structTy = StructType(name, llvmCtx)
            structTy.setBody(type.memberTypes.values.map { lowerType(it) }, packed = false)
            structTy
        }
        is Type.ParamRef ->
            TODO("Can't lower unspecialized type param")
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> requireUnreachable()
        is Type.Constructor -> requireUnreachable()
    }

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "\$string_literal_$nextLiteralIndex"
    }

    private val byteTy = llvm.IntType(8, llvmCtx)
    private val bytePtrTy = llvm.PointerType(byteTy)
    private val voidTy = llvm.VoidType(llvmCtx)
    private val boolTy = llvm.IntType(1, llvmCtx)
    private val cIntTy = llvm.IntType(32, llvmCtx)
    private val trueValue = llvm.ConstantInt(boolTy, 1, false)
    private val falseValue = llvm.ConstantInt(boolTy, 0, false)

    private fun ptrTy(to: llvm.Type): llvm.Type {
        return llvm.PointerType(to)
    }

    private fun linkWithRuntime() {
        log.info("Linking using gcc")
        val commandParts = mutableListOf(
            "gcc",
            "-no-pie",
            "-o", ctx.options.output.toString()
        )
        commandParts.add(ctx.options.runtime.toString())
        commandParts.add(objectFilePath)
        commandParts.addAll(ctx.options.cFlags)

        val outputFile = ctx.options.output.toFile()
        if (outputFile.exists()) {
            outputFile.delete()
        }
        log.info(commandParts.joinToString(" "))
        val builder = ProcessBuilder(commandParts)
        log.debug(builder.command().joinToString(","))
        val process = builder
            .inheritIO()
            .start()
        val exitCode = process.waitFor()
        assert(exitCode == 0) {
            log.error(process.inputStream.readAllBytes().toString(StandardCharsets.UTF_8))
            log.error(process.errorStream.readAllBytes().toString(StandardCharsets.UTF_8))
            log.error("Module: ", LLVM.LLVMPrintModuleToString(llvmModule.ref).string)
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
            llvmModule.ref,
            BytePointer(objectFilePath),
            LLVM.LLVMObjectFile,
            BytePointer("Message")
        )
        LLVM.LLVMRunPassManager(pass, llvmModule.ref)

        LLVM.LLVMDisposePassManager(pass)
        LLVM.LLVMDisposeTargetMachine(targetMachine)
    }

    override fun close() {
        llvmCtx.dispose()
    }
}
