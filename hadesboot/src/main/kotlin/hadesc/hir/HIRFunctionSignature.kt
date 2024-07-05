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
    val type get(): Type {
        val fnType = Type.FunctionPtr(params.map { it.type }, to = returnType, traitRequirements = null)
        return if (typeParams == null) {
            fnType
        } else {
            Type.ForAll(typeParams.map { Type.Param(it.toBinder()) }, fnType)
        }
    }
    fun prettyPrint(): String {
        val typeParamsStr = if (typeParams == null) {
            ""
        } else {
            "<${typeParams.joinToString(", ") { it.prettyPrint() }}> "
        }
        return "fn ${name.mangle()}$typeParamsStr(${params.joinToString(", ") {it.prettyPrint()}})" +
            ": ${returnType.prettyPrint()}"
    }
}
