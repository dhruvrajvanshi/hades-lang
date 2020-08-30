package hadesc.analysis

import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

class TraitResolver(val env: Env) {
    data class Env(val clauses: List<TraitClause>) {
        constructor(vararg clauses: TraitClause): this(listOf(*clauses)) {
        }
    }

    private val typeAnalyzer = TypeAnalyzer()

    fun isTraitImplemented(traitRef: QualifiedName, arguments: List<Type>): Boolean {
        for (clause in env.clauses) {
            if (isTraitImplementedByClause(traitRef, arguments, clause)) {
                return true
            }
        }
        return false
    }

    private fun isTraitImplementedByClause(traitRef: QualifiedName, arguments: List<Type>, clause: TraitClause): Boolean {
        return when (clause) {
            is TraitClause.Implementation -> {
                if (traitRef != clause.traitRef) {
                    return false
                }
                if (clause.arguments.size != arguments.size) {
                    return false
                }
                for ((implType, requiredType) in clause.arguments.zip(arguments)) {
                    val implInstantiation = typeAnalyzer.instantiate(implType, clause.params)
                    if (!(implInstantiation isAssignableTo requiredType)) {
                        return false
                    }
                }
                for (requirement in clause.requirements) {
                    if (!requirement.isSatisfied()) {
                        return false
                    }
                }
                true
            }
        }
    }

    private fun TraitRequirement.isSatisfied(): Boolean {
        TODO()
    }

    private infix fun Type.isAssignableTo(destination: Type): Boolean {
        return typeAnalyzer.isTypeAssignableTo(source = this, destination)
    }

}

sealed class TraitClause {
    data class Implementation(
            val params: List<Type.Param>,
            val traitRef: QualifiedName,
            val arguments: List<Type>,
            val requirements: List<TraitRequirement>
    ): TraitClause()
}

data class TraitRequirement(
        val traitRef: QualifiedName,
        val arguments: List<Type>
)