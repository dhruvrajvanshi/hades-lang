package hadesc.ide.queries

import hadesc.Name
import hadesc.Options
import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.ide.HIDEContext
import hadesc.location.HasLocation
import hadesc.qualifiedname.QualifiedName

data class HQBuild(
        val options: Options
) : HIDEQuery<Unit> {
    override fun run(ctx: HIDEContext): Unit {
        return HQBuildImpl(ctx, this).run()
    }
}

class HQBuildImpl(
        private val ctx: HIDEContext,
        private val thisQuery: HQBuild
) {
    private val options get() = thisQuery.options
    fun run() {
        ctx.rootQuery(HQGetAllSourceFiles(options))
    }

//    fun makeUniqueName(): Name = ctx.makeUniqueName()
//
//    fun makeName(text: String): Name = Name(text)

    private fun forEachSourceFile(function: (SourceFile) -> Unit) {
        allSourceFiles().forEach(function)
    }

    private fun allSourceFiles(): Sequence<SourceFile> {
        return ctx.query(thisQuery, HQGetAllSourceFiles(options)).sequence
    }

    private fun getSourceFileOf(node: HasLocation): SourceFile {
        return requireNotNull(allSourceFiles().find { it.location.file == node.location.file })
    }

    private fun resolveSourceFile(modulePath: QualifiedPath): SourceFile {
        return resolveSourceFile(qualifiedPathToName(modulePath))
    }

    private fun resolveSourceFile(moduleName: QualifiedName): SourceFile {
        return requireNotNull(allSourceFiles().find { it.moduleName == moduleName })
    }

}

private fun qualifiedPathToName(modulePath: QualifiedPath): QualifiedName {
    return QualifiedName(modulePath.identifiers.map { it.name })
}
