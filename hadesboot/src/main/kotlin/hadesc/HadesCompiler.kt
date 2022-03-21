package hadesc

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.path
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.logging.logger
import jdk.jshell.Diag
import java.nio.file.Path
import kotlin.system.exitProcess

sealed interface Options
data class BuildOptions(
    val directories: List<Path>,
    val output: Path,
    val main: Path,
    val runtime: Path,
    val cFlags: List<String>,
    val debugSymbols: Boolean,
    val cSources: List<Path>,
    val dumpLLVMModule: Boolean,
    val libs: List<String>,
    val enableHIRVerifier: Boolean,
) : Options

class HadesCompiler: CliktCommand(name = "hades") {
    private val log = logger()

    private lateinit var options: Options
    private val directories by option("--module-path",
        help = "Add a directory to the module search path").path().multiple()
    private val output by option("--output", "-o").path().required()
    private val main by option("--main").path().required()
    private val cFlags by option("--c-flag").multiple()
    private val debugSymbols by option("--debug-symbols", "-g").flag(default = false)
    private val cSources by option(
        "--c-source",
        help = "Add a C source file to the compilation. Can pass more than once"
    ).path().multiple()
    private val cSourcesSplit by option(
        "--c-sources",
        help = "Add multiple space separated C source files",
    ).path().split(" ").default(emptyList())
    private val dumpLLVMModule by option("--dump-llvm-module").flag(default = false)
    private val libs by option("-l").multiple()
    private val enableHIRVerifier by option("--enable-hir-verifier").flag(default = false)

    override fun run() {
        val hadesHome = System.getenv("HADES_HOME")
        if (hadesHome == null) {
            System.err.println("Environment HADES_HOME must be set to a valid hades install location")
            exitProcess(1)
        }
        options = BuildOptions(
            directories = directories + listOf(Path.of(hadesHome, "stdlib")),
            output = output,
            main = main,
            runtime = Path.of(hadesHome, "stdlib", "runtime.c"),
            cFlags = cFlags,
            debugSymbols = debugSymbols,
            cSources = cSources + cSourcesSplit + listOf(Path.of(hadesHome, "stdlib", "libc.c")),
            dumpLLVMModule = dumpLLVMModule,
            libs = libs,
            enableHIRVerifier = enableHIRVerifier,
        )
    }

    fun execute(): List<Diagnostic> {
        val options = this.options
        check(options is BuildOptions)
        val ctx = Context(options)
        log.debug("Building")
        ctx.build()
        return ctx.diagnosticReporter.errors
    }
}
