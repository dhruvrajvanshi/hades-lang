package hades.ast.parsing

import hades.ast.Span

data class Token(
    val kind: Kind,
    val length: Int,
    val text: String,
    val span: Span,
) {
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
