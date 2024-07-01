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
        println(cSource)
        val commandParts = mutableListOf(cc, "-o", target.output.toString())

        commandParts.add("-L$hadesHome/lib")
        commandParts.add("-I$hadesHome/include")
        commandParts.add("-lgc")
        commandParts.addAll(options.cFlags)
        commandParts.addAll(options.libs.map { "-l$it" })

        commandParts.add(cOutputPath.toString())
        commandParts.addAll(options.cSources.map { it.toString() })

        println(commandParts.joinToString(" "))

        val exitCode = ProcessBuilder()
            .command(*commandParts.toTypedArray())
            .inheritIO()
            .start()
            .waitFor()
        if (exitCode == 0) {
            cOutputPath.deleteIfExists()
        }
        check(exitCode == 0) { "Failed to compile C source" }
    }
}