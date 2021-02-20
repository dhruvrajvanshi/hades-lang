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
        val fnType = Type.Function(params.map { it.type }, to = returnType, traitRequirements = null)
        val ptrType = Type.Ptr(fnType, isMutable = false)
        return if (typeParams == null)
            ptrType
        else
            Type.TypeFunction(typeParams.map { Type.Param(it.toBinder()) }, ptrType)
    }
    fun prettyPrint(): String {
        val typeParamsStr = if (typeParams == null)
            ""
        else "[${typeParams.joinToString(", ") { it.prettyPrint() }}] "
        return "def ${name.mangle()}$typeParamsStr(${params.joinToString(", ") {it.prettyPrint()}})" +
                ": ${returnType.prettyPrint()}"
    }
}
