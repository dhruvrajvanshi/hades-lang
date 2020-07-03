package hadesc.hir

import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

data class HIRInterfaceRef(
        val interfaceName: QualifiedName,
        val typeArgs: List<Type>?
) {
    fun prettyPrint(): String {
        val typeArgsStr = if (typeArgs == null)
            ""
        else "[${typeArgs.joinToString(", ") {it.prettyPrint()}}]"
        return "${interfaceName.mangle()}$typeArgsStr"
    }
}