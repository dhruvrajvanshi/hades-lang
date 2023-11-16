package hadesc.parser

import hadesc.ast.FnSig
import hadesc.ast.Item

fun Parser.parseItem(): Item = when(currentToken.kind) {
    t.PUB -> {
        val start = advance()
        expect(t.FN)
        val ident = parseIdent()
        expect(t.LPAREN)
        expect(t.RPAREN)
        expect(t.ARROW)
        val ty = parseTy()
        val body = parseBlock()

        Item.Fn(
            nodeData(start, body),
            name = ident,
            signature = FnSig(params = emptyList(), output = ty),
            body = body,
        )
    }
    else -> TODO()
}
