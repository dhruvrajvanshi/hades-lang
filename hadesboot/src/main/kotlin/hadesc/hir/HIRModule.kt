package hadesc.hir

import hadesc.qualifiedname.QualifiedName

data class HIRModule(
    val definitions: MutableList<HIRDefinition>
) {
    fun prettyPrint(): String =
        this.definitions.joinToString("\n") { it.prettyPrint() }

    fun findGlobalFunctionDef(name: QualifiedName): HIRDefinition.Function {
        val foundDef = definitions.find {
            it is HIRDefinition.Function && it.name == name
                    && it.receiverType == null
        }
        return foundDef as HIRDefinition.Function
    }

    fun findDefinitions(name: QualifiedName): List<HIRDefinition> {
        return definitions.filter {
            hasName(it, name)
        }
    }

    private fun hasName(definition: HIRDefinition, name: QualifiedName): Boolean = when(definition) {
        is HIRDefinition.Function -> definition.name == name
        is HIRDefinition.ExternFunction -> definition.name == name
        is HIRDefinition.Struct -> definition.name == name
    }

    fun addDefinition(definition: HIRDefinition) {
        definitions.add(definition)
    }
}