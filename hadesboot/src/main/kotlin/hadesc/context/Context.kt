package hadesc.context

import hadesc.*
import hadesc.analysis.Analyzer
import hadesc.ast.*
import hadesc.codegen.HadesBackend
import hadesc.diagnostics.DiagnosticReporter
import hadesc.frontend.Checker
import hadesc.hir.analysis.MissingReturnAnalyzer
import hadesc.hir.analysis.UseAfterMoveAnalyzer
import hadesc.hirgen.HIRGen
import hadesc.hir.passes.*
import hadesc.hir.verifier.HIRVerifier
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Resolver
import hadesc.types.Type
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

sealed interface BuildTarget {

    val mainSourcePath: Path?
    val output: Path?
    data class Executable(
        override val mainSourcePath: Path,
        override val output: Path
    ): BuildTarget

}

class Context(
    val options: BuildOptions,
    val target: BuildTarget
): ASTContext, NamingContext, GlobalConstantContext {
    private val log = logger(Context::class.java)
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
        log.debug("Desugar closures:\n${hirModule.prettyPrint()}")

        hirModule = SimplifyControlFlow(this).transformModule(hirModule)
        log.debug("SimplifyControlFlow:\n${hirModule.prettyPrint()}")

        UseAfterMoveAnalyzer(this).visitModule(hirModule)
        MissingReturnAnalyzer(this).visitModule(hirModule)
        if (diagnosticReporter.hasErrors) {
            return
        }

        hirModule = Monomorphization(this).transformModule(hirModule)
        log.debug("Monomorphization:\n${hirModule.prettyPrint()}")


        val backend = when (options.backend) {
            Backend.LLVM -> HadesBackend.LLVM
            Backend.C -> HadesBackend.LLVM
        }
        backend.generate(this, hirModule)
        unit
    }

    private fun makeSourcePath(path: Path) = SourcePath(path)

    private val parsedSourceFiles = mutableMapOf<Path, SourceFile>()
    fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
        parsedSourceFiles.computeIfAbsent(path.path.toAbsolutePath()) {
            Parser(this, moduleName, path).parseSourceFile()
        }

    fun program(): Program {
        val sourceFiles = buildList {
            forEachSourceFile {
                add(it)
            }
        }
        return Program(sourceFiles)
    }

    fun resolveSourceFile(modulePath: QualifiedPath): SourceFile? {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    private fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
        return QualifiedName(modulePath.identifiers.map { it.name })
    }

    fun resolveSourceFile(moduleName: QualifiedName): SourceFile? {
        if (moduleName.size == 0) {
            val path = target.mainSourcePath
            check(path != null)
            return sourceFile(moduleName, SourcePath(path))
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

        when (target) {
            is BuildTarget.Executable ->
                visitSourceFile(sourceFile(QualifiedName(), makeSourcePath(target.mainSourcePath)))
        }

        visitSourceFile(resolveSourceFile(QualifiedName(listOf(
                makeName("hades"),
                makeName("marker")))))
        visitSourceFile(resolveSourceFile(QualifiedName(listOf(
            makeName("hades"),
            makeName("libhdc")))))
        collectedFiles.values.forEach(action)
    }

    fun qn(vararg names: String) = QualifiedName(names.map { makeName(it) })

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