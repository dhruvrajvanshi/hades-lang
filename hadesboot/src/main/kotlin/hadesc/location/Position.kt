package hadesc.location;

import org.jetbrains.annotations.NotNull;

public record Position(
        int line,
        int column
) implements Comparable<Position> {
    @Override
    public String toString() {
        return "(Line: $line, Column: $column)";
    }

    @Override
    public int compareTo(@NotNull Position other) {
        if (this.gte(other)) {
            return 1;
        } else if (this.lte(other)) {
            return -1;
        } else {
            return 0;
        }
    }

    boolean gte(Position other) {
        if (other.equals(this)) {
            return true;
        }
        if (line == other.line) {
            return column > other.column;
        }
        return line > other.line;
    }

    boolean lte(Position other) {
        if (other.equals(this)) {
            return true;
        }
        if (line == other.line) {
            return column < other.column;
        }

        return line < other.line;
    }
}
