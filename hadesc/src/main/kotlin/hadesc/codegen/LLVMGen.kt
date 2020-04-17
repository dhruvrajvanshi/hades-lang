package hadesc.codegen

import dev.supergrecko.kllvm.core.types.IntType
import dev.supergrecko.kllvm.core.types.PointerType
import dev.supergrecko.kllvm.core.types.StructType
import dev.supergrecko.kllvm.core.values.FunctionValue
import dev.supergrecko.kllvm.core.values.IntValue
import hadesc.context.Context
import hadesc.ir.IRModule
import hadesc.logging.logger
import hadesc.types.Type
import llvm.FunctionType
import llvm.VoidType
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

    }


    private fun verifyModule() {
        // TODO: Handle this in a better way
        val buffer = ByteArray(100)
        val len = LLVM.LLVMVerifyModule(llvmModule.getUnderlyingReference(), 100, buffer)
        println(buffer.decodeToString().slice(0 until len))
    }

    private fun FunctionValue.verify() {
        val validate = LLVM.LLVMVerifyFunction(getUnderlyingReference(), LLVM.LLVMPrintMessageAction)
        if (validate > 0) {
            log.debug("Bad function: ${this.dumpToString()}")
            TODO()
        }
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

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "\$string_literal_$nextLiteralIndex"
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
