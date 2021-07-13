package hadesc.location

import java.nio.file.Path

typealias Line = Int
typealias Column = Int

data class Position(
    val line: Line,
    val column: Column
): Comparable<Position> {
    override fun toString(): String = "(Line: $line, Column: $column)"

    override fun compareTo(other: Position): Int {
        return when {
            this gte other -> {
                1
            }
            this lte other -> {
                -1
            }
            else -> {
                0
            }
        }
    }

    infix fun gte(other: Position): Boolean {
        if (other == this) {
            return true
        }
        if (line == other.line) {
            return column > other.column
        }
        return line > other.line
    }

    infix fun lte(other: Position): Boolean {
        if (other == this) {
            return true
        }
        if (line == other.line) {
            return column < other.column
        }

        return line < other.line
    }
}

data class SourcePath(val path: Path) {
    override fun toString(): String = path.toString()
}

/**
 * Utility class for making type safe Maps of AST nodes
 * based on their location.
 *
 * A TaggedLocation<T> can be only created by passing in
 * a T, despite the fact that it doesn't actually care about
 * the T, only its location.
 * This way, we can have, for example, a Map<TaggedLocation<Binder>, Something>,
 * and be sure that we're only putting SourceLocations of Binder nodes
 * into this map.
 */
class TaggedLocation<T: HasLocation>(node: T) {
    val location = node.location

    override fun equals(other: Any?): Boolean = if (other is TaggedLocation<*>) {
        other.location == location
    } else false

    override fun hashCode(): Int = location.hashCode()
}

inline fun <reified T: HasLocation> T.taggedLocation() =
    TaggedLocation(this)

data class SourceLocation(
    val file: SourcePath,
    val start: Position,
    val stop: Position
) : Comparable<SourceLocation>, HasLocation {
    override val location: SourceLocation
        get() = this

    override fun toString(): String {
        return "($file:${start.line})"
    }

    infix fun isWithin(other: SourceLocation): Boolean {
        return (start gte other.start) && (stop lte other.stop)
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
            if (stop lte other.stop) {
                1
            } else {
                -1
            }

        } else if (start gte other.start) {
            1
        } else {
            -1
        }
    }
}

interface HasLocation {
    val location: SourceLocation
}
