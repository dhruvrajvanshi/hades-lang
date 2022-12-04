package hadesc.context

import hadesc.*
import hadesc.analysis.Analyzer
import hadesc.ast.*
import hadesc.codegen.HIRToLLVM
import hadesc.codegen.LLVMToObject
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

    object None: BuildTarget {
        override val mainSourcePath: Path? = null
        override val output: Path? = null
    }

}

class Context(
    val options: BuildOptions,
    val target: BuildTarget
): ASTContext, NamingContext, GlobalConstantContext {
    private val log = logger(Context::class.java)
    var analyzer = Analyzer(this)
    var resolver = Resolver(this)
    override val enumTagType: Type = Type.u8
    private val modulePathMap by lazy { createModuleMap(::makeName, options.directories) }

    var diagnosticReporter = DiagnosticReporter()
    private var checker = Checker(this)

    override val Expression.type get() = analyzer.typeOfExpression(this)

    fun checkProgram() {
        checker.checkProgram()
    }

    fun build() = profile("Context::build") {
        checkProgram()

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


        val llvmModule = HIRToLLVM(this, hirModule).lower()
        LLVMToObject(options, target, llvmModule).execute()
        unit
    }

    private fun makeSourcePath(path: Path) = SourcePath(path)

    private val parsedSourceFiles = mutableMapOf<Path, SourceFile>()
    private fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
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
            val path = target.mainSourcePath
            check(path != null)
            return sourceFile(moduleName, SourcePath(path))
        }
        val path = modulePathMap[moduleName] ?: return null

        return sourceFile(moduleName, makeSourcePath(path))
    }

    fun forEachSourceFile(action: (SourceFile) -> Unit) {

        when (target) {
            is BuildTarget.Executable ->
                action(sourceFile(QualifiedName(), makeSourcePath(target.mainSourcePath)))
            else -> unit
        }
        for ((moduleName, path) in modulePathMap) {
            action(sourceFile(moduleName, makeSourcePath(path)))
        }
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
    fun onFileChange(path: Path) {
        parsedSourceFiles.remove(path.toAbsolutePath())
        diagnosticReporter = DiagnosticReporter()
        checker = Checker(this)
        resolver = Resolver(this)
        checkProgram()
    }
}