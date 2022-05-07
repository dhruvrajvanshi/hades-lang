package hadesc

import java.nio.file.Path

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
    val dumpHIRGen: Boolean,
    val enableLLVMVerifier: Boolean,
) : Options
