package hades.ast

import hades.URI


data class Position(
    val line: Int,
    val column: Int,
)

data class OffsetSpan(
    val start: Int,
    val stop: Int,
)

interface HasOffsetSpan {
    val offsetSpan: OffsetSpan
}

data class Span(
    val start: Position,
    val stop: Position,
) {
    fun contains(line: Int, column: Int): Boolean {
        return start.line <= line && stop.line >= line
                && start.column <= column && stop.column >= column
    }
}

data class SourceRange(
    val uri: URI,
    val span: Span,
)