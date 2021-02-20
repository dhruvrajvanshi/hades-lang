package hadesc

import hadesc.qualifiedname.QualifiedName

inline class Name constructor(val text: String) {
    fun toQualifiedName() = QualifiedName(listOf(this))
}
