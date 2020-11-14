package hades.ast

interface ASTNode : HasOffsetSpan {
    fun containsOffset(offset: Int): Boolean =
        meta.startOffset <= offset && meta.stopOffset > offset

    val meta: ASTMeta

    override val offsetSpan get() = meta.offsetSpan
}