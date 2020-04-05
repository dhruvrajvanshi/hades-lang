package hadesc.assertions

fun requireUnreachable(lazyMessage: () -> String = { "Compiler bug: Unreachable assertion failed" }): Nothing {
    throw IllegalStateException(lazyMessage())
}
