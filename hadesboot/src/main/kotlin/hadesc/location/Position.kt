package hadesc.location

data class Position(
    val line: Int,
    val column: Int,
) : Comparable<Position> {
    override fun toString(): String {
        return "(Line: $line, Column: $column)"
    }

    override fun compareTo(other: Position): Int {
        return if (gte(other)) {
            1
        } else if (lte(other)) {
            -1
        } else {
            0
        }
    }

    fun gte(other: Position): Boolean {
        if (other == this) {
            return true
        }
        return if (line == other.line) {
            column > other.column
        } else line > other.line
    }

    fun lte(other: Position): Boolean {
        if (other == this) {
            return true
        }
        return if (line == other.line) {
            column < other.column
        } else line < other.line
    }
}