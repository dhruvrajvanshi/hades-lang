package hadesc.hir

import hadesc.qualifiedname.QualifiedName

data class HIRModule(
    val definitions: MutableList<HIRDefinition>
) {
    fun prettyPrint(): String =
        this.definitions.joinToString("\n") { it.prettyPrint() }

    fun findGlobalDefinition(name: QualifiedName): HIRDefinition {
        val foundDef = definitions.find {
            hasName(it, name)
        }
        return requireNotNull(foundDef)
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
        is HIRDefinition.Implementation -> definition.name == name
        is HIRDefinition.Interface -> TODO()
        is HIRDefinition.Const -> definition.name == name
    }

    fun addDefinition(definition: HIRDefinition) {
        definitions.add(definition)
    }
}