package hadesc.parser

import hadesc.ast.Ident
import hadesc.ast.TokenKind

internal typealias t = TokenKind

internal fun Parser.parseIdent(): Ident {
    val token = expect(t.ID)
    return Ident(token.text)
}
