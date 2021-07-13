package hadesc.assertions

fun requireUnreachable(lazyMessage: () -> String = { "Compiler bug: Unreachable assertion failed" }): Nothing {
    System.err.println("Compiler bug")
    throw IllegalStateException(lazyMessage())
}
