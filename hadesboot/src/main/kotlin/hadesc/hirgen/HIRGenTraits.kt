package hadesc.hirgen

import hadesc.analysis.PostAnalysisContext
import hadesc.analysis.TraitRequirement
import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.TraitRequirementAnnotation
import hadesc.context.Context
import hadesc.frontend.PropertyBinding
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIROperand
import hadesc.qualifiedname.QualifiedName

internal class HIRGenTraits(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext,
    private val postAnalysisContext: PostAnalysisContext,
) : HIRGenModuleContext by moduleContext, PostAnalysisContext by postAnalysisContext {

    internal fun lowerImplementationDef(declaration: Declaration.ImplementationDef): List<HIRDefinition> {
        val traitDecl = ctx.resolver.resolveDeclaration(declaration.traitRef)
        require(traitDecl is Declaration.TraitDef)
        return listOf(
            HIRDefinition.Implementation(
                declaration.location,
                typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                traitName = ctx.resolver.qualifiedName(traitDecl.name),
                traitArgs = declaration.traitArguments.map { lowerTypeAnnotation(it) },
                functions = declaration.body.filterIsInstance<Declaration.FunctionDef>().map {
                    lowerFunctionDef(it, QualifiedName(listOf(it.name.identifier.name)))
                },
                typeAliases = declaration.body.filterIsInstance<Declaration.TypeAlias>().associate {
                    require(it.typeParams == null)
                    it.name.name to lowerTypeAnnotation(it.rhs)
                },
                traitRequirements = declaration.whereClause?.traitRequirements?.map {
                    lowerTraitRequirement(it)
                } ?: emptyList()
            )
        )
    }

    internal fun lowerTraitFunctionRef(expression: Expression.Property, binding: PropertyBinding.InterfaceFunctionRef): HIROperand {
        return HIRExpression.TraitMethodRef(
            expression.location,
            expression.type,
            traitName = binding.traitName,
            traitArgs = binding.args,
            methodName = binding.methodName
        )
    }

    private fun lowerTraitRequirement(requirement: TraitRequirementAnnotation): TraitRequirement {
        val traitDef = ctx.resolver.resolveDeclaration(requirement.path)
        require(traitDef is Declaration.TraitDef)
        return TraitRequirement(
            ctx.resolver.qualifiedName(traitDef.name),
            requirement.typeArgs?.map { lowerTypeAnnotation(it) } ?: emptyList(),
            requirement.negated
        )
    }

    internal fun lowerTraitDef(declaration: Declaration.TraitDef): List<HIRDefinition> {
        return emptyList()
    }
}