package hades.ast

import hades.URI


data class ASTMeta(
    val file: URI,
    val startOffset: Int,
    val stopOffset: Int,
) : HasOffsetSpan {
    override val offsetSpan get() = OffsetSpan(startOffset, stopOffset)
}