package hadesc.resolver

import hadesc.ast.ScopeTree
import hadesc.ast.SourceFile

/**
 * A stack of scope, starting from narrow to wide
 */
data class ScopeStack(val scopes: List<ScopeTree>) : Iterable<ScopeTree> {
    private val sourceFile: SourceFile

    init {
        val sourceFileScopeNode = scopes.last()
        if (sourceFileScopeNode !is SourceFile) {
            throw AssertionError("Expected a sourcefile at the end of scope stack")
        }
        sourceFile = sourceFileScopeNode
    }

    override fun iterator(): Iterator<ScopeTree> {
        return scopes.iterator()
    }
}
