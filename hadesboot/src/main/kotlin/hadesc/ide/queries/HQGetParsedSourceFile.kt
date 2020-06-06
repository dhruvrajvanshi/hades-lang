package hadesc.ide.queries

import hadesc.ast.SourceFile
import hadesc.ide.HIDEContext
import hadesc.location.SourcePath
import hadesc.parser.Parser
import hadesc.qualifiedname.QualifiedName

data class HQGetParsedSourceFile(
        val moduleName: QualifiedName,
        val path: SourcePath
) : HIDEQuery<SourceFile> {
    override fun run(ctx: HIDEContext): SourceFile {
        return HQGetParsedSourceFileImpl(ctx, this).run()
    }

}

class HQGetParsedSourceFileImpl(
        private val ctx: HIDEContext,
        private val query: HQGetParsedSourceFile
) {
    fun run(): SourceFile =
        Parser(ctx, query.moduleName, query.path).parseSourceFile()
}
