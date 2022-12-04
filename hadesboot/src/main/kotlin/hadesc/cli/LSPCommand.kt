package hadesc.cli

import com.github.ajalt.clikt.core.CliktCommand
import hadesc.BuildCLIOptions
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import hadesc.context.BuildTarget
import hadesc.context.Context
import hadesc.lsp.HadesLanguageServer
import org.eclipse.lsp4j.launch.LSPLauncher

class LSPCommand: CliktCommand(name = "lsp") {
    private val buildOptions by BuildCLIOptions()

    override fun run() {
        val ctx = Context(
            buildOptions.toBuildOptions(),
            BuildTarget.None,
        )
        val server = HadesLanguageServer(ctx)
        LSPLauncher.createServerLauncher(server, System.`in`, System.out).startListening().get()
    }
}