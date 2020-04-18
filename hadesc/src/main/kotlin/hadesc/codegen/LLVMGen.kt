package hadesc.codegen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.ir.*
import hadesc.logging.logger
import hadesc.types.Type
import llvm.FunctionType
import llvm.StructType
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMTargetMachineRef
import org.bytedeco.llvm.global.LLVM
import java.nio.charset.StandardCharsets

@OptIn(ExperimentalStdlibApi::class)
class LLVMGen(private val ctx: Context, private val irModule: IRModule) : AutoCloseable {
    private val log = logger()
    private val llvmCtx = llvm.Context()
    private val llvmModule = llvm.Module(ctx.options.main.toString(), llvmCtx)
    private val builder = llvm.Builder(llvmCtx)

    fun generate() {
        irModule.definitions.forEach {
            lowerDefinition(it)
        }
        log.debug(LLVM.LLVMPrintModuleToString(llvmModule.getUnderlyingReference()).string)
        verifyModule()
        writeModuleToFile()
        linkWithRuntime()
    }

    private fun lowerDefinition(definition: IRDefinition): Unit =
        when (definition) {
            is IRFunctionDef -> lowerFunctionDef(definition)
            is IRStructDef -> lowerStructDef(definition)
            is IRExternFunctionDef -> lowerExternFunctionDef(definition)
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
                val value = fn.getParam(index)
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
        val basicBlock = fn.appendBasicBlock("entry")
        withinBlock(basicBlock) {
            lowerBlock(definition.body)
        }
    }

    private fun lowerBlock(body: IRBlock) {
        for (statement in body.statements) {
            lowerStatement(statement)
        }
    }

    private fun lowerStatement(statement: IRStatement) = when (statement) {
        is IRValStatement -> lowerValStatement(statement)
        is IRReturnStatement -> lowerReturnStatement(statement)
        IRReturnVoidStatement -> {
            builder.buildRetVoid()
            Unit
        }
        is IRExpression -> {
            lowerExpression(statement)
            Unit
        }
    }

    private val localVariables = mutableMapOf<Name, llvm.Value>()
    private fun lowerValStatement(statement: IRValStatement) {
        val rhs = lowerExpression(statement.initializer)
        val type = lowerType(statement.binder.type)
        val name = lowerName(statement.binder.name)
        val ref = builder.buildAlloca(type, "$name\$_ptr")
        builder.buildStore(rhs, toPointer = ref)
        val value = builder.buildLoad(ref, name)
        localVariables[statement.binder.name] = value
    }

    private fun getLocalVariable(statement: IRValStatement): llvm.Value {
        return requireNotNull(localVariables[statement.binder.name])
    }


    private fun lowerReturnStatement(statement: IRReturnStatement) {
        builder.buildRet(lowerExpression(statement.value))
    }

    private fun lowerExpression(expression: IRExpression) = when (expression) {
        is IRCallExpression -> lowerCallExpression(expression)
        is IRBool -> lowerBoolExpression(expression)
        is IRByteString -> lowerByteString(expression)
        is IRVariable -> lowerVariable(expression)
        is IRGetStructField -> lowerGetStructField(expression)
    }

    private fun lowerGetStructField(expression: IRGetStructField): llvm.Value {
        return builder.buildExtractValue(
            lowerExpression(expression.lhs),
            expression.index,
            lowerName(ctx.makeUniqueName())
        )
    }

    private fun lowerBoolExpression(expression: IRBool): llvm.Value {
        return if (expression.value) {
            trueValue
        } else {
            falseValue
        }
    }

    private fun lowerVariable(expression: IRVariable): llvm.Value =
        when (expression.binding) {
            is IRBinding.FunctionDef -> getDeclaration(expression.binding.def)
            is IRBinding.ExternFunctionDef -> getDeclaration(expression.binding.def)
            is IRBinding.ValStatement -> getLocalVariable(expression.binding.statement)
            is IRBinding.StructDef -> getStructConstructor(expression.binding.def)
            is IRBinding.ParamRef -> {
                val fn = getDeclaration(expression.binding.def)
                fn.getParam(expression.binding.index)
            }
        }

    private fun lowerByteString(expression: IRByteString): llvm.Value {
        val text = expression.value.decodeToString()
        val constStringRef = llvm.ConstantArray(text, nullTerminate = false, context = llvmCtx)
        val globalRef = llvmModule.addGlobal(
            constStringRef.getType(),
            stringLiteralName()
        )
        globalRef.initializer = constStringRef
        return globalRef
    }

    private fun lowerCallExpression(expression: IRCallExpression): llvm.Value {
        val callee = lowerExpression(expression.callee)
        require(expression.typeArgs == null) { "Unspecialized generic function found in LLVMGen" }
        val args = expression.args.map { lowerExpression(it) }
        return builder.buildCall(callee, args)
    }

    private fun withinBlock(basicBlock: llvm.BasicBlock, fn: llvm.Builder.() -> Unit) {
        val pos = builder.getInsertBlock()
        builder.positionAtEnd(basicBlock)
        builder.fn()
        if (pos != null) {
            builder.positionAtEnd(pos)
        }
    }


    private fun getDeclaration(externFunctionDef: IRExternFunctionDef): llvm.FunctionValue {
        val irName = externFunctionDef.externName
        val type = lowerFunctionType(externFunctionDef.type)
        val name = if (irName.text == "main") {
            "hades_main"
        } else {
            lowerName(irName)
        }
        return llvmModule.getFunction(name)?.asFunctionValue() ?: llvmModule.addFunction(name, type)
    }

    private fun getDeclaration(def: IRFunctionDef): llvm.FunctionValue {
        val irName = def.binder.name
        val type = lowerFunctionType(def.type)
        val name = if (irName.text == "main") {
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

    private fun lowerName(name: Name): String {
        return name.text
    }


    private fun verifyModule() {
        // TODO: Handle this in a better way
        val buffer = ByteArray(100)
        val len = LLVM.LLVMVerifyModule(llvmModule.getUnderlyingReference(), 100, buffer)
        println(buffer.decodeToString().slice(0 until len))
    }

    private fun llvm.FunctionValue.verify() {
        val validate = LLVM.LLVMVerifyFunction(ref, LLVM.LLVMPrintMessageAction)
        if (validate > 0) {
            log.debug("Bad function: ${this.dumpToString()}")
            TODO()
        }
    }


    private fun lowerType(type: Type): llvm.Type = when (type) {
        Type.Error -> requireUnreachable()
        Type.Byte -> byteTy
        Type.Void -> voidTy
        is Type.Bool -> boolTy
        is Type.RawPtr -> ptrTy(lowerType(type.to))
        is Type.Function -> {
            require(type.typeParams == null) { "Can't lower unspecialized generic function type" }
            FunctionType(
                returns = lowerType(type.to),
                types = type.from.map { lowerType(it) },
                variadic = false
            )
        }
        is Type.Struct -> StructType(
            type.memberTypes.values.map { lowerType(it) },
            packed = false,
            ctx = llvmCtx
        )
        is Type.ParamRef ->
            TODO("Can't lower unspecialized type param")
        is Type.GenericInstance -> requireUnreachable()
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
    private val trueValue = llvm.ConstantInt(boolTy, 1, false)
    private val falseValue = llvm.ConstantInt(boolTy, 0, false)

    private fun ptrTy(to: llvm.Type): llvm.Type {
        return llvm.PointerType(to)
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
        val outputFile = ctx.options.output.toFile()
        if (outputFile.exists()) {
            outputFile.delete()
        }
        log.info(commandParts.joinToString(" "))
        val builder = ProcessBuilder(commandParts)
        log.debug(builder.command().joinToString(","))
        val process = builder.start()
        val exitCode = process.waitFor()
        assert(exitCode == 0) {
            log.error(process.inputStream.readAllBytes().toString(StandardCharsets.UTF_8))
            log.error(process.errorStream.readAllBytes().toString(StandardCharsets.UTF_8))
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
