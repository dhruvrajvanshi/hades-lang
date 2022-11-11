package hadesc.cli

import com.github.ajalt.clikt.core.CliktCommand
import hadesc.BuildCLIOptions
import com.github.ajalt.clikt.parameters.groups.provideDelegate

class LSPCommand: CliktCommand(name = "lsp") {
    private val buildOptions by BuildCLIOptions()

    override fun run() {
        TODO("Not yet implemented")
    }
}