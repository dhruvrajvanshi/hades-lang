package hadesc.analysis

import hadesc.qualifiedname.QualifiedName
import hadesc.types.Substitution
import hadesc.types.Type

class TraitResolver(private val env: Env, private val typeAnalyzer: TypeAnalyzer) {
    data class Env(val clauses: List<TraitClause>) {
        constructor(vararg clauses: TraitClause): this(listOf(*clauses)) {
        }
    }

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
                val substitution = typeAnalyzer.makeParamSubstitution(clause.params)
                for ((implType, requiredType) in clause.arguments.zip(arguments)) {
                    val implInstantiation = implType.applySubstitution(substitution)
                    if (!(implInstantiation isAssignableTo requiredType)) {
                        return false
                    }
                }
                for (requirement in clause.requirements) {
                    if (!requirement.isSatisfied(substitution)) {
                        return false
                    }
                }
                true
            }
            is TraitClause.Requirement -> {
                if (clause.requirement.traitRef != traitRef) {
                    return false
                }
                if (clause.requirement.arguments.size != arguments.size) {
                    return false
                }
                for ((clauseArg, arg) in clause.requirement.arguments.zip(arguments)) {
                    if (!(clauseArg isAssignableTo arg)) {
                        return false
                    }
                }
                true
            }
        }
    }

    private fun TraitRequirement.isSatisfied(substitution: Substitution): Boolean {
        return isTraitImplemented(traitRef, arguments.map { it.applySubstitution(substitution) })
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
    ): TraitClause() {
        override fun toString(): String {
            return "implementation [${params.joinToString(", ") { it.prettyPrint() } }] : ${traitRef.mangle()}" +
                    "[${arguments.joinToString(", ") { it.prettyPrint() } }] " +
                    "where ${ requirements.joinToString(", ") }"
        }
    }

    data class Requirement(val requirement: TraitRequirement): TraitClause()

}

data class TraitRequirement(
        val traitRef: QualifiedName,
        val arguments: List<Type>
) {
    override fun toString(): String {
        if (arguments.isEmpty()) {
            return traitRef.mangle()
        }
        return "${traitRef.mangle()}[${arguments.joinToString(", ") { it.prettyPrint() } }]"
    }

    fun applySubstitution(substitution: Substitution): TraitRequirement {
        return TraitRequirement(
                traitRef,
                arguments.map { it.applySubstitution(substitution) }
        )
    }

    fun prettyPrint(): String {
        return this.toString()
    }
}