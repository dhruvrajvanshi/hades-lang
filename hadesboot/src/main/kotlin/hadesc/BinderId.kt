package hadesc

/**
 * Globally unique identifier for every name declaration
 * that exists in the source code (or a synthetic id for
 * builtin names).
 * If 2 BinderIds are equal, that means they refer to the same
 * thing (Type/Variable).
 */
@JvmInline
value class BinderId(val value: UInt)