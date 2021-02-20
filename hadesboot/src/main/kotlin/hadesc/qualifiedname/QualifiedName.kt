package hadesc.qualifiedname

import hadesc.Name

data class QualifiedName(
    val names: List<Name> = listOf()
) {
    fun append(name: Name): QualifiedName {
        return QualifiedName(names + name)
    }

    fun mangle(): String = names.joinToString(separator = ".") { it.text }
    fun withPrefix(prefix: QualifiedName): QualifiedName {
        return QualifiedName(prefix.names + names)
    }

    val size get() = names.size

    val first get(): Name = names[0]
}
