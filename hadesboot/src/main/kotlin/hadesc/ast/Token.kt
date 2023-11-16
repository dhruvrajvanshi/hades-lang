package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Token(
    val kind: Kind,
    override val location: SourceLocation,
    val text: String
) : HasLocation {
    enum class Kind {
        PUB,
        FN,
        ID,

        ARROW,
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,

        IMPORT,
        AS,
        EXTERN,
        RETURN,
        VAL,
        STRUCT,
        TRUE,
        FALSE,
        NULLPTR,
        WHILE,
        IF,
        ELSE,
        SIZE_OF,
        ALIGN_OF,
        POINTER_CAST,
        MUT,
        FOR,
        IS,
        ENUM,
        UNION,
        DEFER,
        TYPE,
        WHERE,
        MATCH,
        ARRAY,

        NOT,
        AND,
        OR,

        GREATER_THAN,
        LESS_THAN,
        GREATER_THAN_EQUAL,
        LESS_THAN_EQUAL,

        PLUS,
        MINUS,
        PIPELINE,

        SLASH,
        PERCENT,

        EQEQ,


        INT_LITERAL,
        FLOAT_LITERAL,
        BYTE_CHAR_LITERAL,
        HEX_INT_LITERAL,
        CONST,

        BYTE_STRING,

        DOT,
        SEMICOLON,
        COMMA,
        COLON,
        STAR,
        AMPERSAND,
        LSQB,
        RSQB,
        AT_SYMBOL,
        PRAGMA,
        VBAR,

        EQ,
        BANG_EQ,

        EOF,
        ERROR
    }

    infix fun isA(kind: Token.Kind): Boolean = this.kind == kind
}
typealias TokenKind = Token.Kind
