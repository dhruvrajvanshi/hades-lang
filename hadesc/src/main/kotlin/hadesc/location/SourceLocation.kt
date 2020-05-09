package hadesc.location

import java.nio.file.Path

typealias Line = Int
typealias Column = Int

data class Position(
    val line: Line,
    val column: Column
) {
    override fun toString(): String = "(Line: $line, Column: $column)"

    infix fun greaterThan(other: Position): Boolean {
        if (line == other.line) {
            return column > other.column
        }
        return line > other.line
    }

    infix fun lessThan(other: Position): Boolean {
        if (line == other.line) {
            return column < other.column
        }

        return line < other.line
    }
}

inline class SourcePath(val path: Path)

data class SourceLocation(
    val file: SourcePath,
    val start: Position,
    val stop: Position
) : Comparable<SourceLocation> {
    override fun toString(): String {
        return "($file:${start.line})"
    }

    infix fun isWithin(other: SourceLocation): Boolean {
        return (start greaterThan other.start) && (stop lessThan other.stop)
    }

    infix fun contains(node: HasLocation): Boolean {
        return node.location isWithin this
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

    override fun compareTo(other: SourceLocation): Int {
        return if (start == other.start) {
            if (stop lessThan other.stop) {
                1
            } else {
                -1
            }

        } else if (start greaterThan other.start) {
            1
        } else {
            -1
        }
    }
}

interface HasLocation {
    val location: SourceLocation
}
