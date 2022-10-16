package hadesc.analysis

import hadesc.qualifiedname.QualifiedName
import hadesc.types.Substitution
import hadesc.types.Type
import hadesc.types.emptySubstitution

class TraitResolver<Def>(private val env: Env<Def>, private val typeAnalyzer: TypeAnalyzer) {
    data class Env<Def>(val clauses: List<TraitClause<Def>>) {
        constructor(vararg clauses: TraitClause<Def>): this(listOf(*clauses))
    }

    fun getImplementationClauseAndSubstitution(traitRef: QualifiedName, arguments: List<Type>): Pair<TraitClause<Def>, Substitution>? {
        for (clause in env.clauses) {
            val subst = getImplementationSubstitution(traitRef, arguments, clause)
            if (subst != null) {
                return clause to subst
            }
        }
        return null
    }

    fun isTraitImplemented(traitRef: QualifiedName, arguments: List<Type>): Boolean {
        return getImplementationClauseAndSubstitution(traitRef, arguments) != null
    }


    private fun getImplementationSubstitution(traitRef: QualifiedName, arguments: List<Type>, clause: TraitClause<Def>): Substitution? {
        return when (clause) {
            is TraitClause.Implementation -> {
                if (traitRef != clause.traitRef) {
                    return null
                }
                if (clause.arguments.size != arguments.size) {
                    return null
                }
                val substitution = typeAnalyzer.makeParamSubstitution(clause.params)
                for ((implType, providedType) in clause.arguments.zip(arguments)) {
                    val implInstantiation = implType.applySubstitution(substitution)
                    if (!(providedType isAssignableTo implInstantiation)) {
                        return null
                    }
                }
                for (requirement in clause.requirements) {
                    if (!requirement.isSatisfied(substitution)) {
                        return null
                    }
                }
                substitution
            }
            is TraitClause.Requirement -> {
                if (clause.requirement.traitRef != traitRef) {
                    return null
                }
                if (clause.requirement.arguments.size != arguments.size) {
                    return null
                }
                for ((clauseArg, arg) in clause.requirement.arguments.zip(arguments)) {
                    if (!(clauseArg isAssignableTo arg)) {
                        return null
                    }
                }
                emptySubstitution()
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

sealed class TraitClause<Def> {
    data class Implementation<Def>(
            val params: List<Type.Param>,
            val traitRef: QualifiedName,
            val arguments: List<Type>,
            val requirements: List<TraitRequirement>,
            val def: Def? = null,
    ): TraitClause<Def>() {
        override fun toString(): String {
            return "implementation [${params.joinToString(", ") { it.prettyPrint() } }] : ${traitRef.mangle()}" +
                    "[${arguments.joinToString(", ") { it.prettyPrint() } }] " +
                    "where ${ requirements.joinToString(", ") }"
        }
    }

    data class Requirement<Def>(val requirement: TraitRequirement): TraitClause<Def>()

}

data class TraitRequirement(
        val traitRef: QualifiedName,
        val arguments: List<Type>,
        val negated: Boolean
) {
    override fun toString(): String {
        if (arguments.isEmpty()) {
            return traitRef.mangle()
        }
        val notPrefix = if (negated) "not " else ""
        return "$notPrefix${traitRef.mangle()}[${arguments.joinToString(", ") { it.prettyPrint() } }]"
    }

    fun applySubstitution(substitution: Substitution): TraitRequirement {
        return TraitRequirement(
                traitRef,
                arguments.map { it.applySubstitution(substitution) },
                negated = negated
        )
    }

    fun prettyPrint(): String {
        return this.toString()
    }
}