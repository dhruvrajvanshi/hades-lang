package hadesc

import hadesc.qualifiedname.QualifiedName

@JvmInline
value class Name(val text: String) {
    fun toQualifiedName() = QualifiedName(listOf(this))
}
