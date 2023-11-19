package hadesc.context

import hadesc.ast.QualifiedPath
import hadesc.ast.SourceFile
import hadesc.qualifiedname.QualifiedName

interface SourceFileResolverCtx {
    fun resolveSourceFile(qualifiedPath: QualifiedPath): SourceFile?
    fun resolveSourceFile(qualifiedPath: QualifiedName): SourceFile?
    fun forEachSourceFile(action: (SourceFile) -> Unit)
}