package hadesc

import kotlin.system.exitProcess

fun main(args: Array<String>) {
    val errors = Compiler(args).run()
    if (errors.isNotEmpty()) {
        exitProcess(1)
    }
}