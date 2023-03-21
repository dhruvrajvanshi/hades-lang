package hadesc.vfs

import hadesc.text.Text
import java.nio.file.Path

interface VFS {
    fun read(path: Path): Text
}
