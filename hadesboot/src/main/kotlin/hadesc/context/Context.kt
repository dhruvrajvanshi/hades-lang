package hadesc.context

import hadesc.ast.SourceFile
import hadesc.location.HasLocation
import hadesc.qualifiedname.QualifiedName

//class Context(
//    val options: Options
//) {
//    val checker: Checker = Checker(this)
//    val resolver = Resolver(this)
//    private val collectedFiles = mutableMapOf<SourcePath, SourceFile>()
//
//    val diagnosticReporter = DiagnosticReporter()
//
//    fun build() = profile("Context::build") {
//        forEachSourceFile {
//            for (declaration in it.declarations) {
//                checker.checkDeclaration(declaration)
//            }
//        }
//
//        if (this.diagnosticReporter.hasErrors) {
//            return
//        }
//        var irModule = IRGen(this).generate()
//
//        irModule = ExplicitConstraints(this, irModule).run()
//        irModule = ExplicitThis(this, irModule).run()
//        irModule = SpecializeGenerics(this, irModule).run()
//
//        LLVMGen(this, irModule).use {
//            it.generate()
//        }
//    }
//
//    fun mainPath() = makeSourcePath(options.main)
//
//    private fun makeSourcePath(path: Path) = SourcePath(path)
//
//    private val parsedSourceFiles = mutableMapOf<Path, SourceFile>()
//    fun sourceFile(moduleName: QualifiedName, path: SourcePath) =
//        parsedSourceFiles.computeIfAbsent(path.path.toAbsolutePath()) {
//            TODO()
//            //            Parser(this, moduleName, path).parseSourceFile()
//        }
//
//    fun makeName(text: String): Name = Name(text)
//
//    fun resolveSourceFile(modulePath: QualifiedPath): SourceFile {
//        return resolveSourceFile(qualifiedPathToName(modulePath))
//    }
//
//    fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
//        return QualifiedName(modulePath.identifiers.map { it.name })
//    }
//
//    fun resolveSourceFile(moduleName: QualifiedName): SourceFile {
//        if (moduleName.size == 0) {
//            return sourceFile(moduleName, SourcePath(options.main))
//        }
//        val parts = moduleName.names.joinToString("/") { it.text }
//        val paths = mutableListOf<Path>()
//        for (directory in options.directories) {
//            val path = Path.of(directory.toString(), "$parts.hds")
//            if (path.toFile().exists()) {
//                paths.add(path)
//            }
//        }
//        val moduleNameStr = moduleName.names.joinToString(".") { it.text }
//        if (paths.size == 0) {
//            TODO("No such file $moduleNameStr")
//        } else if (paths.size > 1) {
//            TODO("$moduleNameStr has conflicting files $paths")
//        }
//        return sourceFile(moduleName, makeSourcePath(paths[0]))
//    }
//
//    fun forEachSourceFile(action: (SourceFile) -> Unit) {
//        fun visitSourceFile(sourceFile: SourceFile) {
//            if (collectedFiles.containsKey(sourceFile.location.file)) {
//                return
//            }
//            collectedFiles[sourceFile.location.file] = sourceFile
//            for (declaration in sourceFile.declarations) {
//                if (declaration is Declaration.ImportAs) {
//                    visitSourceFile(resolveSourceFile(declaration.modulePath))
//                }
//            }
//        }
//
//        visitSourceFile(resolveSourceFile(QualifiedName(listOf(makeName("memory")))))
//        visitSourceFile(sourceFile(QualifiedName(), mainPath()))
//        collectedFiles.values.forEach(action)
//    }
//
//    fun getSourceFileOf(node: HasLocation): SourceFile {
//        return requireNotNull(collectedFiles[node.location.file]) {
//            "No source file found for ${node.location.file}"
//        }
//    }
//
//}

