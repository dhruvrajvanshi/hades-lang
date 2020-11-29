package hades.ast

import hades.URI

data class SourceRange(
    val uri: URI,
    val start: Int,
    val stop: Int,
    val documentVersion: Int,
)

data class SourceOffset(
    val uri: URI,
    val offset: Int,
    val documentVersion: Int,
)

interface HasSourceRange {
    val range: SourceRange
}