package hadesc.context

import hadesc.BuildOptions
import hadesc.Name
import hadesc.ast.Declaration
import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.checker.Checker
import hadesc.codegen.LLVMGen
import hadesc.diagnostics.DiagnosticReporter
import hadesc.ir.IRGen
import hadesc.location.HasLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Resolver
import java.nio.file.Path

class Context(
    val options: BuildOptions
) {
    val log = logger()
    val checker: Checker = Checker(this)
    val resolver = Resolver(this)
    private val collectedFiles = mutableMapOf<SourcePath, SourceFile>()

    val diagnosticReporter = DiagnosticReporter()

    fun build() {
        val irModule = IRGen(this).generate()
        log.debug(irModule.prettyPrint())
        LLVMGen(this).use {
            it.generate()
        }
    }

    fun mainPath() = makeSourcePath(options.main)

    private fun makeSourcePath(path: Path) = SourcePath(path)

    fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
        Parser(this, moduleName, path).parseSourceFile()

    fun makeName(text: String): Name = Name(text)

    fun resolveSourceFile(modulePath: QualifiedPath): SourceFile {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
        return QualifiedName(modulePath.identifiers.map { it.name })
    }

    fun resolveSourceFile(moduleName: QualifiedName): SourceFile {
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
        return sourceFile(moduleName, makeSourcePath(paths[0]))
    }

    fun forEachSourceFile(action: (SourceFile) -> Unit) {
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

        visitSourceFile(sourceFile(QualifiedName(), mainPath()))
        collectedFiles.values.forEach(action)
    }

    fun getSourceFileOf(node: HasLocation): SourceFile {
        return requireNotNull(collectedFiles[node.location.file])
    }
}