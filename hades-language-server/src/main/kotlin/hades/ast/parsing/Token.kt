package hades.ast.parsing

import hades.ast.HasSourceRange
import hades.ast.SourceRange

data class Token(
    val kind: Kind,
    val text: String,
    override val range: SourceRange,
) : HasSourceRange {

    enum class Kind {
        ERROR,
        EOF,

        IDENTIFIER,

        DEF, IMPORT, AS, DEFER,
        VAL, RETURN,

        LPAREN, RPAREN,
        LBRACE, RBRACE,
        LSQB, RSQB,

        STAR, COLON, DOT, COMMA,
        EQUAL, EQUALEQUAL,

        SEMICOLON,
    }
}
