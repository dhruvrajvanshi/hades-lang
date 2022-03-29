package hadesc

import hadesc.logging.logger

/**
 * Utility function that's used to force when statements
 * to be treated as expressions to ensure exhaustiveness
 */
fun <T> exhaustive(t: T) = t

inline fun <reified Ctx, T> Ctx.profile(message: String, block: () -> T): T {
    val start = System.currentTimeMillis()
    val result = block()
    val miliseconds = System.currentTimeMillis() - start
    logger().debug("$message took $miliseconds ms")
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

inline fun <reified T> T.ignore(): Unit {}