package hadesc.parser

import hadesc.ast.Item


fun Parser.parseItem(): Item = when(currentToken.kind) {
    else -> TODO()
}
