package hadesc.hir

data class HIRConstraintParam(
        val param: HIRTypeParam,
        val interfaceRef: HIRInterfaceRef
) {
    fun prettyPrint(): String = "${param.name.text} : ${interfaceRef.prettyPrint()}"
}