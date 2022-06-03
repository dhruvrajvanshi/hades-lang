package hadesc.location;

public record SourceLocation(
        SourcePath file,
        Position start,
        Position stop
) implements Comparable<SourceLocation>, HasLocation {

    @Override
    public String toString() {
        return "($file:${start.line})";
    }

    @Override
    public SourceLocation getLocation() {
        return this;
    }

    public boolean isWithin(SourceLocation other) {
        return (start.gte(other.start)) && (stop.lte(other.stop));
    }

    public boolean contains(HasLocation node) {
        return node.getLocation().isWithin(this);
    }

    public static SourceLocation between(HasLocation start, HasLocation stop) {
        return new SourceLocation(
                start.getLocation().file,
                start.getLocation().start,
                stop.getLocation().stop
        );
    }

    @Override
    public int compareTo(SourceLocation other) {
        if (start.equals(other.start)) {
            if (stop.lte(other.stop)) {
                return 1;
            } else {
                return -1;
            }

        } else if (start.gte(other.start)) {
            return 1;
        } else {
            return -1;
        }
    }

}
