package hadesc

import com.charleskorn.kaml.Yaml
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.path
import hadesc.context.BuildTarget
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.logging.logger
import java.io.File
import java.nio.file.Path
import kotlin.io.path.Path
import kotlin.system.exitProcess

class HadesCompiler: CliktCommand(name = "hades") {
    private val log = logger(HadesCompiler::class.java)
    private val fromProjectYML = if (File("hades.yml").exists()) {
        val text = File("hades.yml").readText()
        Yaml.default.decodeFromString(AllOptions.serializer(), text).build
    } else {
        null
    }

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
    private val dumpHIRGen by option("--dump-hirgen").flag(default = false)
    private val libs by option("-l").multiple()
    private val enableHIRVerifier by option("--enable-hir-verifier").flag(default = false)
    private val enableLLVMVerifier by option("--enable-llvm-verifier").flag(default = false)

    override fun run() {
        val hadesHome = System.getenv("HADES_HOME")
        if (hadesHome == null) {
            System.err.println("Environment HADES_HOME must be set to a valid hades install location")
            exitProcess(1)
        }
        val yamlDirectories = fromProjectYML?.directories?.map { Path(it) } ?: emptyList()
        val yamlCFlags = fromProjectYML?.cFlags ?: emptyList()
        val yamlCSources = fromProjectYML?.cSources?.map { Path(it) } ?: emptyList()
        val yamlLibs = fromProjectYML?.libs ?: emptyList()
        options = BuildOptions(
            directories = yamlDirectories + directories + listOf(Path.of(hadesHome, "stdlib")),
            runtime = Path.of(hadesHome, "stdlib", "runtime.c"),
            cFlags = yamlCFlags + cFlags,
            debugSymbols = debugSymbols,
            cSources = yamlCSources + cSources + cSourcesSplit + listOf(
                Path.of(hadesHome, "stdlib", "libc.c"),
                Path.of(hadesHome, "stdlib", "libhdc.c")
            ),
            dumpLLVMModule = dumpLLVMModule,
            libs = yamlLibs + libs,
            enableHIRVerifier = enableHIRVerifier,
            dumpHIRGen = dumpHIRGen,
            enableLLVMVerifier = enableLLVMVerifier,
        )
    }

    fun execute(): List<Diagnostic> {
        val options = this.options
        check(options is BuildOptions)
        val ctx = Context(options, BuildTarget.Executable(
            mainSourcePath = main,
            output = output
        ))
        log.debug("Building")
        ctx.build()
        return ctx.diagnosticReporter.errors
    }
}
