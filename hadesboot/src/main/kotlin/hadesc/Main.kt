package hadesc

import hadesc.cli.Build
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    val compiler = Build()
    compiler.main(args)
    val diagnostics = compiler.execute()
    if (diagnostics.isNotEmpty()) {
        System.err.println("Compilation errors detected")
        exitProcess(1)
    }
}