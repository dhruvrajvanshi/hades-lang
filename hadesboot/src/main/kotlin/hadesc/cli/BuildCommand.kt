package hadesc.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.help
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.path
import hadesc.BuildCLIOptions
import hadesc.logging.logger
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.encodeToStream
import java.io.File
import java.nio.file.Paths
import kotlin.system.exitProcess

class BuildCommand : CliktCommand(
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

    override fun run() = TODO()
}
