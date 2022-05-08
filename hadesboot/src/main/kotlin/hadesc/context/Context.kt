package hadesc.context

import hadesc.BuildOptions
import hadesc.Name
import hadesc.ast.Declaration
import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.analysis.Analyzer
import hadesc.ast.Expression
import hadesc.codegen.HIRToLLVM
import hadesc.codegen.LLVMToObject
import hadesc.diagnostics.DiagnosticReporter
import hadesc.frontend.Checker
import hadesc.hirgen.HIRGen
import hadesc.hir.passes.*
import hadesc.hir.verifier.HIRVerifier
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.parser.Parser
import hadesc.profile
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Resolver
import hadesc.types.Type
import hadesc.unit
import java.nio.file.Path

interface ASTContext {
    val Expression.type: Type
}
interface NamingContext {
    fun makeUniqueName(prefix: String = ""): Name
    fun makeName(text: String): Name = Name(text)
}
interface GlobalConstantContext {
    val enumTagType: Type
}
class Context(
    val options: BuildOptions
): ASTContext, NamingContext, GlobalConstantContext {
    val analyzer = Analyzer(this)
    val resolver = Resolver(this)
    private val collectedFiles = mutableMapOf<SourcePath, SourceFile>()
    override val enumTagType: Type = Type.u8

    val diagnosticReporter = DiagnosticReporter()

    override val Expression.type get() = analyzer.typeOfExpression(this)

    fun build() = profile("Context::build") {
        Checker(this).checkProgram()

        if (this.diagnosticReporter.hasErrors) {
            return
        }

        var hirModule = HIRGen(this).lowerSourceFiles(parsedSourceFiles.values)
        if (this.diagnosticReporter.hasErrors) {
            return
        }
        if (options.dumpHIRGen) {
            print("HIRGen output")
            print(hirModule.prettyPrint())
        }
        if (options.enableHIRVerifier) {
            HIRVerifier(hirModule).verify()
        }

        hirModule = DesugarClosures(this).transformModule(hirModule)
        logger().debug("Desugar closures:\n${hirModule.prettyPrint()}")

        hirModule = SimplifyControlFlow(this).transformModule(hirModule)
        logger().debug("SimplifyControlFlow:\n${hirModule.prettyPrint()}")

        hirModule = Monomorphization(this).transformModule(hirModule)
        logger().debug("Monomorphization:\n${hirModule.prettyPrint()}")


        val llvmModule = HIRToLLVM(this, hirModule).lower()
        LLVMToObject(options, llvmModule).execute()
        unit
    }

    private fun mainPath() = makeSourcePath(options.main)

    private fun makeSourcePath(path: Path) = SourcePath(path)

    private val parsedSourceFiles = mutableMapOf<Path, SourceFile>()
    fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
        parsedSourceFiles.computeIfAbsent(path.path.toAbsolutePath()) {
            Parser(this, moduleName, path).parseSourceFile()
        }


    fun resolveSourceFile(modulePath: QualifiedPath): SourceFile? {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    private fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
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
        visitSourceFile(resolveSourceFile(QualifiedName(listOf(
            makeName("hades"),
            makeName("libhdc")))))
        collectedFiles.values.forEach(action)
    }

    private var _nameIndex = 0
    override fun makeUniqueName(prefix: String): Name {
        _nameIndex++
        if (prefix.isNotBlank()) {
            return makeName("\$$prefix\$$_nameIndex")
        }
        return makeName("\$$_nameIndex")
    }

    fun enumTagType(): Type = enumTagType
}