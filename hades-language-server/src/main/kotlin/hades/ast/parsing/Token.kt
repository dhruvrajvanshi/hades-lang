package hades.ast.parsing

import hades.ast.HasOffsetSpan
import hades.ast.OffsetSpan
import hades.ast.Span

data class Token(
    val kind: Kind,
    val text: String,
    val startOffset: Int,
    val stopOffset: Int,
    val span: Span,
) : HasOffsetSpan {
    override val offsetSpan get() = OffsetSpan(startOffset, stopOffset)

    enum class Kind {
        ERROR,
        EOF,

        IDENTIFIER,

        DEF, IMPORT, AS, DEFER,
        VAL, RETURN,

        LPAREN, RPAREN,
        LBRACE, RBRACE,
        LSQB, RSQB,

        STAR, COLON, DOT,

        SEMICOLON,
    }
}
