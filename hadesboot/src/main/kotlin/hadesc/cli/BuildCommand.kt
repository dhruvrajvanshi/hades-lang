package hadesc.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.help
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.path
import hadesc.context.BuildTarget
import hadesc.BuildCLIOptions
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.logging.logger
import kotlinx.serialization.ExperimentalSerializationApi
import java.io.File
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.encodeToStream
import java.nio.file.Paths
import kotlin.system.exitProcess

class BuildCommand: CliktCommand(
    invokeWithoutSubcommand = true,
    epilog = "Build complete"
) {
    private val log = logger(BuildCommand::class.java)
    private val buildOptions by BuildCLIOptions()
    private val output by option("--output", "-o").path().required()
    private val main by option("--main").path().required()
    private val skipExec by option("--internal-skip-exec")
        // we have this flag because HadesTestSuite
        // needs to call execute manually to get the list of diagnostics
        // as a List
        .help("Internal flag. To be used only by the implementation").flag()

    override fun run() {
        if (skipExec) {
            return
        }
        val diagnostics = execute()
        if (diagnostics.isNotEmpty()) {
            println("Build failed with errors")
            exitProcess(1)
        }
    }

    fun execute(): List<Diagnostic> {
        val options = buildOptions.toBuildOptions()
        val ctx = Context(options, BuildTarget.Executable(
            mainSourcePath = main,
            output = output
        ))
        log.debug("Building")
        ctx.build()

        if (options.jsonDiagnostics) {
            emitJSONDiagnostics(ctx.diagnosticReporter.errors)
        }
        return ctx.diagnosticReporter.errors
    }

    @OptIn(ExperimentalSerializationApi::class)
    private fun emitJSONDiagnostics(errors: List<Diagnostic>) {
        val dotHadesDir = File(".hades")
        if (dotHadesDir.exists() && !dotHadesDir.isDirectory) {
            dotHadesDir.delete()
        }
        if (!dotHadesDir.exists()) {
            dotHadesDir.mkdir()
        }

        val diagnosticsJSONFile = Paths.get(dotHadesDir.path, "diagnostics.json").toFile()

        if (diagnosticsJSONFile.exists()) {
            diagnosticsJSONFile.delete()
        }

        diagnosticsJSONFile.outputStream().buffered().use {
            Json.encodeToStream(errors, it)
        }
    }
}
