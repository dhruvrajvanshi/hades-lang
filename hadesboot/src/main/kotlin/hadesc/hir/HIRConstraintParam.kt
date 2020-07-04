package hadesc.hir

import hadesc.types.Type

data class HIRConstraintParam(
        val type: Type,
        val param: HIRTypeParam,
        val interfaceRef: HIRInterfaceRef
) {
    fun prettyPrint(): String = "${param.name.text} : ${interfaceRef.prettyPrint()}"
}