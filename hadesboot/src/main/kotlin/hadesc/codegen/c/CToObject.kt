package hadesc.codegen.c

import hadesc.context.BuildTarget
import kotlin.io.path.Path
import kotlin.io.path.createDirectory
import kotlin.io.path.exists
import kotlin.io.path.writeText

class CToObject(private val cSource: String, private val target: BuildTarget) {
    fun execute() {
        if (!Path(".hades").exists()) {
            Path(".hades").createDirectory()
        }
        val cOutputPath = Path(".hades/temp_${System.currentTimeMillis()}.c")
        cOutputPath.writeText(cSource)
        println(cSource)
        val exitCode = ProcessBuilder()
            .command("clang", "-o", target.output.toString(), cOutputPath.toString())
            .inheritIO()
            .start()
            .waitFor()
        check(exitCode == 0) { "Failed to compile C source" }
    }
}