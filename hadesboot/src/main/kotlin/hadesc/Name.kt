package hadesc

import hadesc.qualifiedname.QualifiedName

inline class Name(val text: String) {
    fun toQualifiedName() = QualifiedName(listOf(this))
}
