package hadesc.location

import java.nio.file.Path

typealias Line = Int
typealias Column = Int

data class Position(
    val line: Line,
    val column: Column
) {
    override fun toString(): String = "(Line: $line, Column: $column)"
}

inline class SourcePath(val path: Path) {
    override fun toString(): String = path.toString()
}

data class SourceLocation(
    val file: SourcePath,
    val start: Position,
    val stop: Position
) {
    override fun toString(): String {
        return "($file:${start.line})"
    }

    companion object {
        @JvmStatic
        fun between(start: HasLocation, stop: HasLocation): SourceLocation {
            return SourceLocation(
                file = start.location.file,
                start = start.location.start,
                stop = stop.location.stop
            )
        }
    }
}

interface HasLocation {
    val location: SourceLocation
}
