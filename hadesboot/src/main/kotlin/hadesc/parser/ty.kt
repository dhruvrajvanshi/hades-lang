package hadesc.parser

import hadesc.ast.Ty

internal fun Parser.parseTy(): Ty {
    val start = expect(t.LPAREN)
    val stop = expect(t.RPAREN)
    return Ty.Tup(
        nodeData(start, stop),
        emptyList(),
    )
}