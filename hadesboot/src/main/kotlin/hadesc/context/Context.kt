package hadesc.context

import hadesboot.middle.lower.lowerToC
import hadesc.Backend
import hadesc.BuildOptions
import hadesc.analysis.Analyzer
import hadesc.analysis.typecheck
import hadesc.ast.*
import hadesc.codegen.HIRToLLVM
import hadesc.codegen.LLVMToObject
import hadesc.codegen.c.CToObject
import hadesc.codegen.c.HIRToC
import hadesc.diagnostics.DiagnosticReporter
import hadesc.frontend.Checker
import hadesc.hir.analysis.MissingReturnAnalyzer
import hadesc.hir.analysis.UseAfterMoveAnalyzer
import hadesc.hir.passes.*
import hadesc.hir.verifier.HIRVerifier
import hadesc.hirgen.HIRGen
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Resolver
import hadesc.text.Text
import hadesc.types.Type
import hadesc.unit
import java.io.File
import java.nio.file.Path

interface ASTContext {
    val Expression.type: Type
}

sealed interface BuildTarget {

    val mainSourcePath: Path?
    val output: Path?
    data class Executable(
        override val mainSourcePath: Path,
        override val output: Path
    ) : BuildTarget
}

class Context(
    val options: BuildOptions,
    val target: BuildTarget,
    private val fileTextProvider: FileTextProvider = FileSystemFileTextProvider,
    private val idGenCtx: IdGenCtx = IdGenCtxImpl(),
    private val namingCtx: NamingCtx = NamingCtxImpl(),
    private val globalConstantsCtx: GlobalConstantsCtx = GlobalConstantsCtxImpl()
) : ASTContext,
    SourceFileResolverCtx,
    ResolverCtx,
    DiagnosticReporterCtx,
    GlobalConstantsCtx by globalConstantsCtx,
    IdGenCtx by idGenCtx,
    NamingCtx by namingCtx
{
    private val log = logger(Context::class.java)
    override val resolver = Resolver(this)
    val analyzer = Analyzer(this)
    private val collectedFiles = mutableMapOf<SourcePath, SourceFile>()

    override val diagnosticReporter = DiagnosticReporter(fileTextProvider)


    override val Expression.type get() = analyzer.typeOfExpression(this)
    fun check() {
        if (options.enableNewTypeChecker) {
            val sourceFiles = buildList {
                forEachSourceFile { add(it) }
            }
            typecheck(sourceFiles, diagnosticReporter)
        }
        Checker(this).checkProgram()
    }

    fun build() {
        check()
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

        hirModule = DesugarClosures(this, this).transformModule(hirModule)
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

        when (options.backend) {
            Backend.C -> {
                val cSource = HIRToC(hirModule, enableDebug = options.debugSymbols).lower()
                CToObject(cSource, target, options).execute()
            }
            Backend.LLVM -> {
                val llvmModule = HIRToLLVM(this, hirModule).lower()
                LLVMToObject(options, target, llvmModule).execute()
            }
        }

        unit
    }

    private fun makeSourcePath(path: Path) = SourcePath(path)

    private val parsedSourceFiles = mutableMapOf<Path, SourceFile>()
    private fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
        parsedSourceFiles.computeIfAbsent(path.path.toAbsolutePath()) {
            Parser(
                ctx = this,
                moduleName = moduleName,
                resolver = resolver,
                diagnosticReporter = diagnosticReporter,
                text = fileTextProvider.getFileText(path.path),
                file = path,
            ).parseSourceFile()
        }

    override fun resolveSourceFile(modulePath: QualifiedPath): SourceFile? {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    private fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
        return QualifiedName(modulePath.identifiers.map { it.name })
    }

    override fun resolveSourceFile(moduleName: QualifiedName): SourceFile? {
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

    override fun forEachSourceFile(action: (SourceFile) -> Unit) {
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

        visitSourceFile(stdlibSource("hades.marker"))
        visitSourceFile(stdlibSource("hades.libhdc"))
        visitSourceFile(stdlibSource("hades.internal.gc"))
        collectedFiles.values.forEach(action)
    }

    private fun stdlibSource(name: String) =
        resolveSourceFile(
            QualifiedName(name.split(".").map { makeName(it) })
        )

    fun enumTagType(): Type = enumTagType
}

interface FileTextProvider {
    fun getFileText(path: Path): Text
}
object FileSystemFileTextProvider : FileTextProvider {
    override fun getFileText(path: Path): Text {
        return Text.from(File(path.toUri()).readText())
    }
}