package hadesc

import hadesc.qualifiedname.QualifiedName
import java.nio.file.Path
import kotlin.io.path.*

fun createModuleMap(makeName: (String) -> Name, roots: List<Path>): Map<QualifiedName, Path> = buildMap {
    val visitDirSet = mutableSetOf<Path>()
    fun visitFile(parentModule: QualifiedName, path: Path) {
        val thisModuleName = parentModule.append(makeName(path.nameWithoutExtension))
        if (contains(thisModuleName)) {
            return
        }
        put(thisModuleName, path)
    }
    fun visitDirectory(parentModule: QualifiedName, path: Path) {
        if (path.toAbsolutePath() in visitDirSet) {
            return
        } else {
            visitDirSet.add(path.toAbsolutePath())
        }
        check(path.exists())
        check(path.isDirectory())

        val thisModuleName = parentModule.append(makeName(path.name))
        for (child in path.listDirectoryEntries()) {
            if (child.isDirectory()) {
                visitDirectory(thisModuleName, child)
            } else if (child.extension == "hds") {
                visitFile(thisModuleName, child)
            }
        }
    }

    fun visitRoot(path: Path) {
        check(path.exists())
        check(path.isDirectory())

        for (child in path.listDirectoryEntries()) {
            if (child.isDirectory()) {
                visitDirectory(QualifiedName(), child)
            } else if (child.extension == "hds") {
                visitFile(QualifiedName(), child)
            }
        }
    }
    for (root in roots) {
        visitRoot(root)
    }
}
