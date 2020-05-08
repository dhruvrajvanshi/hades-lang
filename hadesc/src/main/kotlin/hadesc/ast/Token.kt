package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Token(
    val kind: Kind,
    override val location: SourceLocation,
    val text: String
) : HasLocation {
    enum class Kind {
        IMPORT,
        DEF,
        AS,
        EXTERN,
        RETURN,
        VAL,
        STRUCT,
        TRUE,
        FALSE,
        THIS,
        NULLPTR,
        WHILE,
        NOT,
        IF,
        ELSE,

        ID,
        INT_LITERAL,

        BYTE_STRING,

        DOT,
        SEMICOLON,
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,
        COMMA,
        COLON,
        STAR,
        LSQB,
        RSQB,

        EQ,

        EOF,
        ERROR,
    }
}
typealias TokenKind = Token.Kind