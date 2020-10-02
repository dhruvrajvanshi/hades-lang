package hadesc.hir

import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

data class HIRFunctionSignature(
        val location: SourceLocation,
        val name: QualifiedName,
        val typeParams: List<HIRTypeParam>?,
        val params: List<HIRParam>,
        val returnType: Type
) {
    fun prettyPrint(): String {
        val typeParamsStr = if (typeParams == null)
            ""
        else "[${typeParams.joinToString(", ") { it.prettyPrint() }}] "
        return "def ${name.mangle()}$typeParamsStr(${params.joinToString(", ") {it.prettyPrint()}})" +
                ": ${returnType.prettyPrint()}"
    }
}
