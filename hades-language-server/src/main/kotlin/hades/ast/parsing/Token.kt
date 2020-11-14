package hades.ast.parsing

data class Token(
    val kind: Kind,
    val text: String,
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
