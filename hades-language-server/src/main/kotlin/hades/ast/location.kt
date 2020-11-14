package hades.ast

import hades.URI


data class Position(
    val line: Int,
    val column: Int,
)

data class Span(
    val start: Position,
    val stop: Position,
)

data class SourceRange(
    val uri: URI,
    val span: Span,
)