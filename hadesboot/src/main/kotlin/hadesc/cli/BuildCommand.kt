package hadesc.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.path
import hadesc.context.BuildTarget
import hadesc.BuildCLIOptions
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.ignore
import hadesc.logging.logger

class BuildCommand: CliktCommand(invokeWithoutSubcommand = true) {
    private val log = logger(BuildCommand::class.java)
    private val buildOptions by BuildCLIOptions()
    private val output by option("--output", "-o").path().required()
    private val main by option("--main").path().required()

    override fun run() = execute().ignore()

    fun execute(): List<Diagnostic> {
        val options = buildOptions.toBuildOptions()
        val ctx = Context(options, BuildTarget.Executable(
            mainSourcePath = main,
            output = output
        ))
        log.debug("Building")
        ctx.build()
        return ctx.diagnosticReporter.errors
    }
}
