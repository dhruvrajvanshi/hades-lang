package hadesc

/**
 * Utility function that's used to force when statements
 * to be treated as expressions to ensure exhaustiveness
 */
fun <T> exhaustive(t: T) = t

inline fun <reified Ctx, T> Ctx.profile(message: String, block: () -> T): T {
    val start = System.currentTimeMillis()
    val result = block()
    val miliseconds = System.currentTimeMillis() - start
    return result
}

fun <T> scoped(builder: Scoped.() -> T): T {
    val defer = Scoped()
    val result = defer.builder()
    defer.done()
    return result
}

class Scoped {
    internal val blocks = mutableListOf<() -> Unit>()
}

internal fun Scoped.done() {
    blocks.reversed().forEach { it() }
}
fun Scoped.defer(block: () -> Unit) {
    blocks.add(block)
}

val unit = Unit

inline fun <reified T> T.ignore() {}

fun clampToPowerOfTwo(value: Int): Int {
    var currentValue = 1
    while (currentValue < value) {
        currentValue = currentValue shl 1
    }
    return currentValue
}

fun <T> Collection<T>.dropLast() = asSequence().take(size - 1)
