package hadesc.assertions

fun assert(condition: Boolean) {
    if (!condition) {
        throw AssertionError()
    }
}