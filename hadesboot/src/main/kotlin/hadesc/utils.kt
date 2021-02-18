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
fun printIndent(indent: Int) = (0..(indent * 2)).joinToString("") { " " }

val unit = Unit