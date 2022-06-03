package hadesc.location

import java.nio.file.Path

data class SourcePath(
    val path: Path
) {
    override fun toString(): String {
        return path.toString()
    }
}