package hadesc.context

import hadesc.BuildOptions
import hadesc.Name
import hadesc.ast.Declaration
import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.analysis.Analyzer
import hadesc.ast.Expression
import hadesc.codegen.IRToLLVMGen
import hadesc.diagnostics.DiagnosticReporter
import hadesc.frontend.Checker
import hadesc.hir.HIRGen
import hadesc.hir.passes.DesugarClosures
import hadesc.hir.passes.DesugarWhenExpressions
import hadesc.hir.passes.SystemVABILowering
import hadesc.hir.passes.Monomorphization
import hadesc.irgen.IRGen
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.parser.Parser
import hadesc.profile
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Resolver
import hadesc.types.Type
import java.nio.file.Path
interface HasContext {
    val ctx: Context

    val Expression.type get() = ctx.analyzer.typeOfExpression(this)
}
class Context(
    val options: BuildOptions
) {
    val analyzer = Analyzer(this)
    val resolver = Resolver(this)
    private val collectedFiles = mutableMapOf<SourcePath, SourceFile>()

    val diagnosticReporter = DiagnosticReporter()

    fun build() = profile("Context::build") {
        Checker(this).checkProgram()

        if (this.diagnosticReporter.hasErrors) {
            return
        }

        var hirModule = HIRGen(this).lowerSourceFiles(parsedSourceFiles.values)
        if (this.diagnosticReporter.hasErrors) {
            return
        }
        hirModule = DesugarWhenExpressions(this).transformModule(hirModule)
        hirModule = DesugarClosures(this).transformModule(hirModule)
//        val hirChecker = HIRChecker().checkModule(hirModule)
        logger().debug("Desugar closures:\n${hirModule.prettyPrint()}")

        hirModule = Monomorphization(this).transformModule(hirModule)
        hirModule = SystemVABILowering(this).transformModule(hirModule)
        val irModule = IRGen(this).generate(hirModule)

        IRToLLVMGen(this, irModule).use {
            it.generate()
        }
    }

    fun mainPath() = makeSourcePath(options.main)

    private fun makeSourcePath(path: Path) = SourcePath(path)

    private val parsedSourceFiles = mutableMapOf<Path, SourceFile>()
    fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
        parsedSourceFiles.computeIfAbsent(path.path.toAbsolutePath()) {
            Parser(this, moduleName, path).parseSourceFile()
        }

    fun makeName(text: String): Name = Name(text)

    fun resolveSourceFile(modulePath: QualifiedPath): SourceFile? {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
        return QualifiedName(modulePath.identifiers.map { it.name })
    }

    fun resolveSourceFile(moduleName: QualifiedName): SourceFile? {
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
            return null
        } else if (paths.size > 1) {
            TODO("$moduleNameStr has conflicting files $paths")
        }
        return sourceFile(moduleName, makeSourcePath(paths[0]))
    }

    fun forEachSourceFile(action: (SourceFile) -> Unit) {
        fun visitSourceFile(sourceFile: SourceFile?) {
            if (sourceFile == null) {
                return
            }
            if (collectedFiles.containsKey(sourceFile.location.file)) {
                return
            }
            collectedFiles[sourceFile.location.file] = sourceFile
            for (declaration in sourceFile.declarations) {
                if (declaration is Declaration.ImportAs) {
                    visitSourceFile(resolveSourceFile(declaration.modulePath))
                }
                if (declaration is Declaration.ImportMembers) {
                    visitSourceFile(resolveSourceFile(declaration.modulePath))
                }
            }
        }

        visitSourceFile(sourceFile(QualifiedName(), mainPath()))

        visitSourceFile(resolveSourceFile(QualifiedName(listOf(
                makeName("hades"),
                makeName("marker")))))
        collectedFiles.values.forEach(action)
    }

    private var _nameIndex = 0
    fun makeUniqueName(): Name {
        _nameIndex++
        return makeName("\$$_nameIndex")
    }

    fun sealedTypeDiscriminantType(): Type = Type.Integral(64, false)
}