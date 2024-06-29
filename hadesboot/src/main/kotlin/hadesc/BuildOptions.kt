package hadesc

import com.charleskorn.kaml.Yaml
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.enum
import com.github.ajalt.clikt.parameters.types.path
import kotlinx.serialization.Serializable
import java.io.File
import java.nio.file.Path
import kotlin.io.path.Path
import kotlin.system.exitProcess

sealed interface Options

@Serializable
data class HadesYAMLOptions(
    val build: YamlBuildOptions
)

@Serializable
data class YamlBuildOptions(
    val cSources: List<String> = emptyList(),
    val cFlags: List<String> = emptyList(),
    val libs: List<String> = emptyList(),
    val directories: List<String> = emptyList(),
    val jsonDiagnostics: Boolean = false,
    val backend: Backend = Backend.LLVM,
)

enum class Backend {
    LLVM,
    C,
}
data class BuildOptions(
    val directories: List<Path>,
    val runtime: Path,
    val cFlags: List<String>,
    val debugSymbols: Boolean,
    val cSources: List<Path>,
    val dumpLLVMModule: Boolean,
    val libs: List<String>,
    val enableHIRVerifier: Boolean,
    val dumpHIRGen: Boolean,
    val enableLLVMVerifier: Boolean,
    val jsonDiagnostics: Boolean,
    val backend: Backend,
) : Options

class BuildCLIOptions : OptionGroup() {
    private val directories by option(
        "--module-path",
        help = "Add a directory to the module search path"
    ).path().multiple()
    private val cFlags by option("--c-flag").multiple()
    private val debugSymbols by option("--debug-symbols", "-g").flag(default = false)
    private val cSources by option(
        "--c-source",
        help = "Add a C source file to the compilation. Can pass more than once"
    ).path().multiple()
    private val cSourcesSplit by option(
        "--c-sources",
        help = "Add multiple space separated C source files"
    ).path().split(" ").default(emptyList())
    private val dumpLLVMModule by option("--dump-llvm-module").flag(default = false)
    private val dumpHIRGen by option("--dump-hirgen").flag(default = false)
    private val libs by option("-l").multiple()
    private val enableHIRVerifier by option("--enable-hir-verifier").flag(default = false)
    private val enableLLVMVerifier by option("--enable-llvm-verifier").flag(default = false)
    private val jsonDiagnostics by option("--json-diagnostics")
        .flag(default = false)
        .help("Emit errors and warnings to .hades/diagnostics.json")
    private val backend by option("--backend").enum<Backend>().default(Backend.LLVM)

    private val fromProjectYML = if (File("hades.yml").exists()) {
        val text = File("hades.yml").readText()
        Yaml.default.decodeFromString(HadesYAMLOptions.serializer(), text).build
    } else {
        null
    }

    fun toBuildOptions(): BuildOptions {
        val hadesHome = System.getenv("HADES_HOME")
        if (hadesHome == null) {
            System.err.println("Environment HADES_HOME must be set to a valid hades install location")
            exitProcess(1)
        }
        val yamlDirectories = fromProjectYML?.directories?.map { Path(it) } ?: emptyList()
        val yamlCFlags = fromProjectYML?.cFlags ?: emptyList()
        val yamlCSources = fromProjectYML?.cSources?.map { Path(it) } ?: emptyList()
        val yamlLibs = fromProjectYML?.libs ?: emptyList()
        return BuildOptions(
            directories = yamlDirectories + directories + listOf(Path.of(hadesHome, "stdlib")),
            runtime = Path.of(hadesHome, "stdlib", "runtime.c"),
            cFlags = yamlCFlags + cFlags,
            debugSymbols = debugSymbols,
            cSources = yamlCSources + cSources + cSourcesSplit + listOf(
                Path.of(hadesHome, "stdlib", "libc.c"),
                Path.of(hadesHome, "stdlib", "libhdc.c"),
                Path.of(hadesHome, "stdlib", "hades", "internal", "gc.c")
            ),
            dumpLLVMModule = dumpLLVMModule,
            libs = yamlLibs + libs,
            enableHIRVerifier = enableHIRVerifier,
            dumpHIRGen = dumpHIRGen,
            enableLLVMVerifier = enableLLVMVerifier,
            jsonDiagnostics = jsonDiagnostics,
            backend = backend,
        )
    }
}
