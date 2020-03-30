package hadesc.context

import hadesc.BuildOptions
import hadesc.Name
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
}