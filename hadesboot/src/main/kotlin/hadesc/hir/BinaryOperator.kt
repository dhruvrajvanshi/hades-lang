package hadesc.hir

enum class BinaryOperator {
    PLUS,
    MINUS,
    TIMES,

    AND,
    OR,

    EQUALS,
    NOT_EQUALS,

    GREATER_THAN,
    GREATER_THAN_EQUAL,

    LESS_THAN,
    LESS_THAN_EQUAL;

    fun prettyPrint(): String = when(this) {
        PLUS -> "add"
        MINUS -> "sub"
        TIMES -> "mul"
        AND -> "and"
        OR -> "or"
        EQUALS -> "eq"
        NOT_EQUALS -> "neq"
        GREATER_THAN -> "gt"
        GREATER_THAN_EQUAL -> "gte"
        LESS_THAN -> "lt"
        LESS_THAN_EQUAL -> "lte"
    }
}