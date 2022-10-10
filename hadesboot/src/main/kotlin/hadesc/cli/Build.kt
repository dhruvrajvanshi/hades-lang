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
import hadesc.logging.logger
import hadesc.unit

class Build: CliktCommand(invokeWithoutSubcommand = true) {
    private val log = logger(Build::class.java)
    private val buildOptions by BuildCLIOptions()
    private val output by option("--output", "-o").path().required()
    private val main by option("--main").path().required()

    // just parse options. Run execute separately to actually run
    override fun run() = unit

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
