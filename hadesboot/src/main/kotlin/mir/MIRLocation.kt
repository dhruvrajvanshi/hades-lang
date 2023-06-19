package mir

import java.nio.file.Path

data class MIRLocation(
    val line: Int,
    val column: Int,
    val file: Path,
)