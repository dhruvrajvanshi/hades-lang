package hadesc

/**
 * Utility function that's used to force when statements
 * to be treated as expressions to ensure exhaustiveness
 */
fun <T> exhaustive(t: T) = t