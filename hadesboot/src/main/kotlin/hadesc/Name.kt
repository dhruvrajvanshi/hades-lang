package hadesc

import hadesc.qualifiedname.QualifiedName
import kotlin.jvm.JvmInline

@JvmInline
value class Name constructor(val text: String) {
    fun toQualifiedName() = QualifiedName(listOf(this))
}
