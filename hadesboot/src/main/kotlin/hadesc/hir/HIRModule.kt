package hadesc.hir

data class HIRModule(
    val definitions: List<HIRDefinition>
) {
    fun prettyPrint(): String =
        this.definitions.joinToString("\n") { it.prettyPrint() }
}