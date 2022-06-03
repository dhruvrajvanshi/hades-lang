package hadesc.location;
import java.nio.file.Path;

public record SourcePath(
        Path path
) {
    @Override
    public String toString() {
        return path.toString();
    }
}
