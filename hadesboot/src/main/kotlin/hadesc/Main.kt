package hadesc

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.subcommands
import hadesc.cli.BuildCommand
import hadesc.cli.LSPCommand
import hadesc.cli.TestCommand

class MainCommand: CliktCommand(name = "hades") {
    override fun run() = unit
}
fun main(args: Array<String>) {
    val compiler = MainCommand().subcommands(BuildCommand(), TestCommand(), LSPCommand())
    compiler.main(args)
}