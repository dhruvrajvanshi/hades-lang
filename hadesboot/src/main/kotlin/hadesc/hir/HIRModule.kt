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
        return requireNotNull(foundDef) {
            "No def with name: ${name.mangle()}"
        }
    }

    fun findDefinitions(name: QualifiedName): List<HIRDefinition> {
        return definitions.filter {
            hasName(it, name)
        }
    }

    fun findDefinition(name: QualifiedName): HIRDefinition {
        return findDefinitions(name).let {
            check(it.size == 1) {
                "Definition $name not found in module."
            }
            it.first()
        }
    }

    private fun hasName(definition: HIRDefinition, name: QualifiedName): Boolean = when (definition) {
        is HIRDefinition.Function -> definition.name == name
        is HIRDefinition.ExternFunction -> definition.name == name
        is HIRDefinition.Struct -> definition.name == name
        is HIRDefinition.Const -> definition.name == name
        is HIRDefinition.ExternConst -> definition.name == name
    }

    fun addDefinition(definition: HIRDefinition) {
        definitions.add(definition)
    }

    fun findStructDef(structName: QualifiedName): HIRDefinition.Struct {
        return findDefinition(structName)
            .let {
                check(it is HIRDefinition.Struct)
                it
            }
    }

    fun findStructDefOrNull(structName: QualifiedName): HIRDefinition.Struct? {
        return findDefinitions(structName)
            .filterIsInstance<HIRDefinition.Struct>()
            .firstOrNull()
    }

    fun findGlobalFunction(name: QualifiedName): HIRDefinition.Function {
        return findDefinition(name)
            .let {
                check(it is HIRDefinition.Function)
                it
            }
    }
}
