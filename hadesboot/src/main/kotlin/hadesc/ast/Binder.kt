package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

/**
 * A binder is separated a type level just so
 * that we can distinguish between identifiers
 * that bind a name in a scope and regular
 * variables that refer to these bindings.
 */
@JvmInline
value class Binder(
    val identifier: Identifier
) : HasLocation {
    override val location: SourceLocation
        get() = identifier.location

    val name get() = identifier.name
}