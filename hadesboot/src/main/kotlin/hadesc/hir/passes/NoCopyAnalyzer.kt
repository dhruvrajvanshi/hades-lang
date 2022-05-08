package hadesc.hir.passes

import hadesc.analysis.TraitClause
import hadesc.analysis.TraitRequirement
import hadesc.analysis.TraitResolver
import hadesc.analysis.TypeAnalyzer
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.diagnostics.DiagnosticReporter
import hadesc.hir.AbstractHIRCFGVisitor
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRModule
import hadesc.hir.HIRStatement
import hadesc.location.HasLocation
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
    override fun visitStore(statement: HIRStatement.Store) {
        val rhsType = statement.value.type
        verifyIsCopyable(statement.value, rhsType)
        super.visitStore(statement)
    }

    override fun visitAssignmentStatement(statement: HIRStatement.Assignment) {
        val rhsType = statement.value.type
        verifyIsCopyable(statement.value, rhsType)
        super.visitAssignmentStatement(statement)
    }

    private fun verifyIsCopyable(node: HasLocation, type: Type) {
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
        val traitResolver = TraitResolver(env, TypeAnalyzer())

        if (traitResolver.isTraitImplemented(
             ctx.qn("hades", "marker", "NoCopy"),
            listOf(type)
        )) {
            diagnosticReporter.report(node.location, Diagnostic.Kind.CanNotCopyNoCopyType(type))
        }
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