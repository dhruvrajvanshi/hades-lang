package hadesc.hir.passes

import hadesc.Name
import hadesc.analysis.TraitClause
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitResolver
import hadesc.analysis.TypeAnalyzer
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.diagnostics.DiagnosticReporter
import hadesc.hir.*
import hadesc.types.Type

/**
 * Analysis pass that checks for ownership violations like
 * attempting to copy a NoCopy type and use after moves.
 */
class NoCopyAnalyzer(
    private val ctx: Context,
    module: HIRModule,
    private val diagnosticReporter: DiagnosticReporter
): AbstractHIRCFGVisitor(module) {
    private val isMoved = mutableSetOf<Name>()
    override fun beforeRun() {
        for (structDef in module.definitions.filterIsInstance<HIRDefinition.Struct>()) {
            generateImplicityCopyImpl(structDef)
        }
        print("After auto derivation of Copy")
        print(module.prettyPrint())
    }

    /**
     * Implicitly generate copy implementation if all members are copy
     */
    private fun generateImplicityCopyImpl(structDef: HIRDefinition.Struct) {
        if (structDef.typeParams == null) {
            if (structDef.fields.all { (_, ty) -> isTypeTriviallyCopyable(ty) }) {
                module.definitions.add(
                    HIRDefinition.Implementation(
                        structDef.location,
                        traitRequirements = emptyList(),
                        typeParams = null,
                        traitName = ctx.qn("hades", "marker", "Copy"),
                        traitArgs = listOf(structDef.instanceType()),
                        functions = emptyList(),
                        typeAliases = emptyMap()
                    )
                )
            }
        } else {
            val requirements = mutableListOf<TraitRequirement>()
            structDef.fields.forEach { (_, ty) ->
                if (isTypeTriviallyCopyable(ty)) {
                    return@forEach
                } else {
                    requirements.add(
                        TraitRequirement(
                            traitRef = ctx.qn("hades", "marker", "Copy"),
                            arguments = listOf(ty)
                        )
                    )
                }
            }
            if (requirements.isNotEmpty()) {
                module.definitions.add(
                    HIRDefinition.Implementation(
                        structDef.location,
                        traitRequirements = requirements,
                        typeParams = structDef.typeParams,
                        traitName = ctx.qn("hades", "marker", "Copy"),
                        traitArgs = listOf(structDef.instanceType(structDef.typeParams.map { Type.ParamRef(it.toBinder()) })),
                        functions = emptyList(),
                        typeAliases = emptyMap(),
                    )
                )
            }
        }

    }

    override fun visitStore(statement: HIRStatement.Store) {
        verifyIsCopyable(statement.value)
        super.visitStore(statement)
    }

    override fun visitAssignmentStatement(statement: HIRStatement.Assignment) {
        verifyIsCopyable(statement.value)
        super.visitAssignmentStatement(statement)
    }

    /**
     * This method returns true for builtin or non-aggregate
     * types.
     */
    private fun isTypeTriviallyCopyable(type: Type): Boolean {
        return when (type) {
            is Type.Ptr,
            is Type.Integral,
            is Type.Bool,
            is Type.Size,
            is Type.FloatingPoint,
            is Type.Void -> true
            else -> false
        }
    }

    private fun verifyIsCopyable(expr: HIRExpression) {
        if (expr !is HIRExpression.LocalName) {
            return
        }
        val type = expr.type

        if (!isTypeCopyable(type)) {
            diagnosticReporter.report(expr.location, Diagnostic.Kind.CanNotCopyNoCopyType(type))
        }
    }

    private fun isTypeCopyable(type: Type): Boolean {
        return makeTraitResolver().isTraitImplemented(
            ctx.qn("hades", "marker", "Copy"),
            listOf(type)
        )
    }

    private fun makeTraitResolver(): TraitResolver<HIRDefinition.Implementation> {
        val allImpls = module.definitions.filterIsInstance<HIRDefinition.Implementation>()
        // FIXME: HIRFunction doesn't store trait requirements right now.
        //        Pass them in HIRGen and add them to the env

        val currentImpl = implementationDef
        val currentImplRequirements = currentImpl?.traitRequirements ?: emptyList()
        val currentImplClauses = currentImplRequirements.map { it.toClause() }
        val allImplsClauses = allImpls.map { it.toClause() }
        val env = TraitResolver.Env(
            currentImplClauses + allImplsClauses
        )
        return TraitResolver(env, TypeAnalyzer())
    }

    private fun TraitRequirement.toClause(): TraitClause<HIRDefinition.Implementation> {
        return TraitClause.Requirement(this)
    }

    private fun HIRDefinition.Implementation.toClause(): TraitClause<HIRDefinition.Implementation> {
        return TraitClause.Implementation(
            params = typeParams?.map { Type.Param(Binder(Identifier(it.location, it.name))) } ?: emptyList(),
            traitRef = traitName,
            arguments = traitArgs,
            requirements = traitRequirements,
            def = this
        )
    }
}