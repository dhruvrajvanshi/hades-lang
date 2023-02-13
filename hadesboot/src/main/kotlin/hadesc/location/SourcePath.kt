package hadesc.location

import kotlinx.serialization.Serializable
import java.nio.file.Path


@Serializable
data class SourcePath(
    val path: Path
) {
    override fun toString(): String {
        return path.toString()
    }
}