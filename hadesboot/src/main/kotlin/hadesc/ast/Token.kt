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
        IF,
        ELSE,
        SIZE_OF,
        POINTER_CAST,
        MUT,
        INTERFACE,
        IMPLEMENT,
        THIS_TYPE,
        FOR,
        MATCH,
        ENUM,
        NEW,

        NOT,
        AND,
        OR,

        GREATER_THAN,
        LESS_THAN,
        GREATER_THAN_EQUAL,
        LESS_THAN_EQUAL,

        PLUS,
        MINUS,

        EQEQ,

        ARROW,

        ID,
        INT_LITERAL,
        HEX_INT_LITERAL,
        CONST,

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
        AMPERSAND,
        LSQB,
        RSQB,

        EQ,
        BANG_EQ,

        EOF,
        ERROR,
    }
}
typealias TokenKind = Token.Kind