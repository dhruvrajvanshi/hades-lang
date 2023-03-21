package hadesc.vfs

import hadesc.text.Text
import java.nio.file.Path

interface VFS {
    fun read(path: Path): Text
}

class FileSystemVFS : VFS {
    override fun read(path: Path): Text {
        return Text(path.toFile().readText())
    }
}
