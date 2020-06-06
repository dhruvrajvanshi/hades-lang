package hadesc.ide.queries

import hadesc.Options
import hadesc.ast.Declaration
import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.ide.HIDEContext
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import java.nio.file.Path

class AllSourceFiles(
        private val sourceFiles: Map<SourcePath, SourceFile>
) {
    public val sequence get() = sourceFiles.values.asSequence()
}

data class HQGetAllSourceFiles(
        val options: Options
): HIDEQuery<AllSourceFiles> {
    override fun run(ctx: HIDEContext): AllSourceFiles {
        return HQGetAllSourceFilesImpl(ctx, this).run()
    }
}

private class HQGetAllSourceFilesImpl(
        private val ctx: HIDEContext,
        private val thisQuery: HQGetAllSourceFiles
) {
    fun run(): AllSourceFiles {
        val collectedFiles = mutableMapOf<SourcePath, SourceFile>()
        fun visitSourceFile(sourceFile: SourceFile) {
            if (collectedFiles.containsKey(sourceFile.location.file)) {
                return
            }
            collectedFiles[sourceFile.location.file] = sourceFile
            for (declaration in sourceFile.declarations) {
                if (declaration is Declaration.ImportAs) {
                    visitSourceFile(resolveSourceFile(declaration.modulePath))
                }
            }
        }

        visitSourceFile(resolveSourceFile(QualifiedName(listOf(ctx.makeName("memory")))))
        visitSourceFile(sourceFile(QualifiedName(), SourcePath(options.main)))
        return AllSourceFiles(collectedFiles)
    }

    private val options get() = thisQuery.options

    private fun resolveSourceFile(moduleName: QualifiedName): SourceFile {
        if (moduleName.size == 0) {
            return sourceFile(moduleName, SourcePath(options.main))
        }
        val parts = moduleName.names.joinToString("/") { it.text }
        val paths = mutableListOf<Path>()
        for (directory in options.directories) {
            val path = Path.of(directory.toString(), "$parts.hds")
            if (path.toFile().exists()) {
                paths.add(path)
            }
        }
        val moduleNameStr = moduleName.names.joinToString(".") { it.text }
        if (paths.size == 0) {
            TODO("No such file $moduleNameStr")
        } else if (paths.size > 1) {
            TODO("$moduleNameStr has conflicting files $paths")
        }
        return sourceFile(moduleName, SourcePath(paths[0]))
    }

    private fun sourceFile(moduleName: QualifiedName, path: SourcePath): SourceFile {
        return ctx.query(thisQuery, HQGetParsedSourceFile(moduleName, path))
    }

    private fun resolveSourceFile(modulePath: QualifiedPath): SourceFile {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    private fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
        return QualifiedName(modulePath.identifiers.map { it.name })
    }
}

