package hadesc.hir

import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

data class HIRFunctionSignature(
        val location: SourceLocation,
        val name: QualifiedName,
        val typeParams: List<HIRTypeParam>?,
        val constraintParams: List<HIRConstraintParam>?,
        val params: List<HIRParam>,
        val returnType: Type
) {
    init {
        require(constraintParams == null || constraintParams.isNotEmpty())
    }
    fun prettyPrint(): String {
        val whereStr = if (constraintParams == null) {
            ""
        } else {
            "where (${constraintParams.joinToString(", ") {it.prettyPrint()} })"
        }
        val typeParamsStr = if (typeParams == null)
            ""
        else "[${typeParams.joinToString(", ") { it.prettyPrint() }}] $whereStr "
        return "def ${name.mangle()}$typeParamsStr(${params.joinToString(", ") {it.prettyPrint()}})" +
                ": ${returnType.prettyPrint()}"
    }
}
