package hadesc.location

data class SourceLocation(
    val file: SourcePath,
    val start: Position,
    val stop: Position,
) : Comparable<SourceLocation>, HasLocation {
    override fun toString(): String {
        return "(" + file + ":" + start.line() + ")"
    }

    override val location: SourceLocation
        get() = this

    fun isWithin(other: SourceLocation): Boolean {
        return start.gte(other.start) && stop.lte(other.stop)
    }

    operator fun contains(node: HasLocation): Boolean {
        return node.location.isWithin(this)
    }

    override fun compareTo(other: SourceLocation): Int {
        return if (start == other.start) {
            if (stop.lte(other.stop)) {
                1
            } else {
                -1
            }
        } else if (start.gte(other.start)) {
            1
        } else {
            -1
        }
    }

    companion object {
        fun between(start: HasLocation, stop: HasLocation): SourceLocation {
            return SourceLocation(
                start.location.file,
                start.location.start,
                stop.location.stop
            )
        }
    }
}