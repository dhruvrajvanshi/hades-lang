package hadesc.codegen

import hadesc.BuildOptions
import hadesc.context.BuildTarget
import hadesc.logging.logger
import llvm.ref
import org.apache.commons.lang3.SystemUtils
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMModuleRef
import org.bytedeco.llvm.LLVM.LLVMTargetRef
import org.bytedeco.llvm.global.LLVM
import java.nio.file.Path
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteExisting

private val hadesHome = System.getenv("HADES_HOME")
class LLVMToObject(private val options: BuildOptions, private val target: BuildTarget, private val llvmModule: LLVMModuleRef) {
    private val log = logger(LLVMToObject::class.java)
    private val objectFilePath get() = target.output.toString() + if (shouldUseMicrosoftCL) ".obj" else ".o"

    private val cc = when {
        SystemUtils.IS_OS_WINDOWS -> System.getenv()["CC"] ?: "cl"
        SystemUtils.IS_OS_MAC_OSX -> System.getenv()["CC"] ?: "clang"
        else -> System.getenv()["CC"] ?: "gcc"
    }

    private val shouldUseMicrosoftCL = cc == "cl" || cc == "cl.exe"

    fun execute() {
        writeModuleToFile()
        linkWithRuntime()
        Path.of(objectFilePath).deleteExisting()
    }

    private fun linkWithRuntime() {
        log.debug("Linking using $cc")
        val commandParts = mutableListOf(cc)
        if (options.debugSymbols) {
            if (!shouldUseMicrosoftCL) {
                commandParts.add("-g")
            } else {
                commandParts.add("/DEBUG")
            }
        } else {
            if (!SystemUtils.IS_OS_WINDOWS) {
                // -flto doesn't work on windows GCC, that's why this condition isn't `shouldUseMicrosoftCL`
                commandParts.add("-flto")
            }
            if (!shouldUseMicrosoftCL) {
                commandParts.add("-O2")
            } else {
                commandParts.add("/O2")
                commandParts.add("/GL")
                commandParts.add("/GF")
                commandParts.add("/Gw")
            }
        }

        if (SystemUtils.IS_OS_WINDOWS && !shouldUseMicrosoftCL) {
            commandParts.add("-D")
            commandParts.add("__HDC_CHKSTK_UNAVAILABLE")
        }

        if (shouldUseMicrosoftCL) {
            commandParts.add("/Fe\"${target.output}\"")
        } else {
            commandParts.add("-o")
            commandParts.add(target.output.toString())
        }

        commandParts.addAll(options.cSources.map { it.toString() })
        commandParts.add(options.runtime.toString())
        commandParts.add(objectFilePath)
        commandParts.addAll(options.cFlags)
        commandParts.addAll(options.libs.map { "-l$it" })

        if (shouldUseMicrosoftCL) {
            commandParts.add("/MD")
            commandParts.add("-I")
            commandParts.add("$hadesHome/include")
        } else {
            commandParts.add("-L$hadesHome/lib")
            commandParts.add("-I$hadesHome/include")
        }

        val outputFile = target.output?.toFile()
        check(outputFile != null)
        if (outputFile.exists()) {
            outputFile.delete()
        }
        log.debug(commandParts.joinToString(" "))
        val builder = ProcessBuilder(commandParts)

        val process = builder
            .inheritIO()
            .start()
        val exitCode = process.waitFor()
        check(exitCode == 0) {
            "${commandParts.joinToString(" ")} exited with code $exitCode"
        }
    }

    private fun writeModuleToFile() {
        val output = target.output
        check(output != null)
        makeParentDirectory(output)
        log.debug("Writing object file")
        log.debug(LLVM.LLVMPrintModuleToString(llvmModule).string)
        // With MSVC, (as of org.bytedeco:llvm-platform:13.0.1-1.5.7),
        // LLVMVerifyModule randomly crashes. Disable this to make CI
        // for windows succeed consistently.
        if (!shouldUseMicrosoftCL) {
            LLVM.LLVMVerifyModule(llvmModule, LLVM.LLVMAbortProcessAction, ByteArray(10000))
        }
        LLVM.LLVMInitializeAllTargetInfos()
        LLVM.LLVMInitializeAllTargets()
        LLVM.LLVMInitializeAllTargetMCs()
        LLVM.LLVMInitializeAllAsmParsers()
        LLVM.LLVMInitializeAllAsmPrinters()
        val targetTriple = LLVM.LLVMGetDefaultTargetTriple()
        val cpu = BytePointer("generic")
        val features = BytePointer("")
        val targetRef = LLVMTargetRef()

        val targetRefErrorMessage = BytePointer()
        val targetRefError = LLVM.LLVMGetTargetFromTriple(LLVM.LLVMGetDefaultTargetTriple(), targetRef, targetRefErrorMessage)
        check(targetRefError == 0) {
            targetRefErrorMessage.string
        }

        val targetMachine = LLVM.LLVMCreateTargetMachine(targetRef, targetTriple, cpu, features, LLVM.LLVMCodeGenLevelDefault, LLVM.LLVMRelocDefault, LLVM.LLVMCodeModelDefault)

        if (!options.debugSymbols) {
            val pass = LLVM.LLVMCreatePassManager()
            LLVM.LLVMAddFunctionInliningPass(pass)
            LLVM.LLVMAddPromoteMemoryToRegisterPass(pass)
            LLVM.LLVMAddAggressiveDCEPass(pass)
            LLVM.LLVMAddFunctionInliningPass(pass)
            LLVM.LLVMAddGlobalDCEPass(pass)
            LLVM.LLVMAddGlobalOptimizerPass(pass)
            LLVM.LLVMRunPassManager(pass, llvmModule.ref)
            LLVM.LLVMDisposePassManager(pass)
        }

        if (options.dumpLLVMModule) {
            LLVM.LLVMPrintModuleToFile(llvmModule, "$objectFilePath.ll", null as BytePointer?)
        }

        val error = BytePointer()
        val failure = LLVM.LLVMTargetMachineEmitToFile(
            targetMachine,
            llvmModule.ref,
            BytePointer(objectFilePath),
            LLVM.LLVMObjectFile,
            BytePointer("Message")
        )
        if (failure != 0) {
            System.err.println(error.string)
            LLVM.LLVMDisposeErrorMessage(error)
        }
        check(failure == 0)

        LLVM.LLVMDisposeTargetMachine(targetMachine)
    }
}

private fun makeParentDirectory(output: Path) {
    output.toAbsolutePath().parent.createDirectories()
}
