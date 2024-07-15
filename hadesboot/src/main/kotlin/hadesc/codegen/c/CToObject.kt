package hadesc.codegen.c

import hadesc.BuildOptions
import hadesc.context.BuildTarget
import org.apache.commons.lang3.SystemUtils
import kotlin.io.path.*

private val hadesHome = System.getenv("HADES_HOME")
class CToObject(private val cSource: String, private val target: BuildTarget, private val options: BuildOptions) {
    private val cc = when {
        SystemUtils.IS_OS_WINDOWS -> System.getenv()["CC"] ?: "cl"
        SystemUtils.IS_OS_MAC_OSX -> System.getenv()["CC"] ?: "clang"
        else -> System.getenv()["CC"] ?: "gcc"
    }

    private val shouldUseMicrosoftCL = cc == "cl" || cc == "cl.exe"
    fun execute() {
        if (!Path(".hades").exists()) {
            Path(".hades").createDirectory()
        }
        val cOutputPath = Path(".hades/temp_${System.currentTimeMillis()}.c")
        cOutputPath.writeText(cSource)

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
        commandParts.add(cOutputPath.toString())
        commandParts.addAll(options.cFlags)
        commandParts.addAll(options.libs.map { "-l$it" })


        println(commandParts.joinToString(" "))

        val exitCode = ProcessBuilder()
            .command(*commandParts.toTypedArray())
            .inheritIO()
            .start()
            .waitFor()
        if (exitCode == 0 && !options.preserveCSource) {
            cOutputPath.deleteIfExists()
        }
        check(exitCode == 0) { "Failed to compile C source" }
    }
}