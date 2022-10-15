package hadesc

import kotlinx.serialization.Serializable
import java.nio.file.Path

sealed interface Options

@Serializable
data class AllOptions(
    val build: YamlBuildOptions
)

@Serializable
data class YamlBuildOptions(
    val cSources: List<String> = emptyList(),
    val cFlags: List<String> = emptyList(),
    val libs: List<String> = emptyList(),
    val directories: List<String> = emptyList(),
)

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
) : Options
