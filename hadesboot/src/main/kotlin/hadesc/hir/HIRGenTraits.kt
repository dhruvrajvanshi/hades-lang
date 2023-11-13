package hadesc.hir

import hadesc.analysis.TraitRequirement
import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.TraitRequirementAnnotation
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.frontend.PropertyBinding
import hadesc.hirgen.HIRGenModuleContext
import hadesc.qualifiedname.QualifiedName

internal class HIRGenTraits(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext,
) : HIRGenModuleContext by moduleContext, ASTContext by ctx {
}