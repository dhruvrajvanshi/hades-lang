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
        FN,
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
        THIS,
        EXTENSION,
        MATCH,
        MOVE,
        REF,
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

        ARROW,

        ID,
        INT_LITERAL,
        FLOAT_LITERAL,
        BYTE_CHAR_LITERAL,
        HEX_INT_LITERAL,
        CONST,

        CSTRING,

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
        AT_SYMBOL,
        AT_INTRINSIC,
        PRAGMA,
        VBAR,

        EQ,
        BANG_EQ,

        EOF,
        ERROR
    }
}
typealias TokenKind = Token.Kind
