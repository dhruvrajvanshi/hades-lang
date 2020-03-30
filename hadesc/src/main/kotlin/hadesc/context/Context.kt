package hadesc.context

import hadesc.BuildOptions
import hadesc.Name
import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.checker.Checker
import hadesc.codegen.LLVMGen
import hadesc.diagnostics.DiagnosticReporter
import hadesc.location.SourcePath
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Resolver
import java.nio.file.Path

class Context(
    val options: BuildOptions
) {
    val checker: Checker = Checker(this)
    val resolver = Resolver()

    val diagnosticReporter = DiagnosticReporter()

    fun build() {
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
        val parts = modulePath.identifiers.map { it.name.text }.joinToString("/")
        val paths = mutableListOf<Path>()
        for (directory in options.directories) {
            val path = Path.of(directory.toString(), "$parts.hds")
            if (path.toFile().exists()) {
                paths.add(path)
            }
        }
        val moduleName = QualifiedName(modulePath.identifiers.map { it.name })
        val moduleNameStr = modulePath.identifiers.joinToString(".") { it.name.text }
        if (paths.size == 0) {
            TODO("No such file $moduleNameStr")
        } else if (paths.size > 1) {
            TODO("$moduleNameStr has conflicting files $paths")
        }
        return sourceFile(moduleName, makeSourcePath(paths[0]))

        TODO()
    }
}