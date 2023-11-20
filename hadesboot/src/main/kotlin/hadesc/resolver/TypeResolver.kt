package hadesc.resolver

import hadesc.ast.*
import hadesc.context.SourceFileResolverCtx


/**
 * Find [ident] in [scopeNode]
 * Only considers bindings that are introduced by [scopeNode] itself, not its child/parent scopes
 */
internal fun findTypeInScope(ctx: SourceFileResolverCtx, ident: Identifier, scopeNode: ScopeTree): TypeBinding? = when (scopeNode) {
    is Declaration.FunctionDef -> {
        findTypeInFunctionDef(ident, scopeNode)
    }
    is SourceFile -> {
        findTypeInSourceFile(ctx, ident, scopeNode)
    }
    is Block -> null
    is Declaration.Struct -> {
        val param = scopeNode.typeParams?.findLast {
            it.binder.identifier.name == ident.name
        }
        when {
            param != null -> {
                TypeBinding.TypeParam(param.binder)
            }
            ident.name == scopeNode.binder.identifier.name -> {
                TypeBinding.Struct(scopeNode)
            }
            else -> {
                null
            }
        }
    }
    is Declaration.Enum -> {
        val param = scopeNode.typeParams?.findLast {
            it.binder.identifier.name == ident.name
        }
        if (param != null) {
            TypeBinding.TypeParam(param.binder)
        } else {
            null
        }
    }
    is Declaration.TypeAlias -> {
        val param = scopeNode.typeParams?.find {
            it.binder.identifier.name == ident.name
        }
        if (param != null) TypeBinding.TypeParam(param.binder) else null
    }
    is Declaration.ExtensionDef -> {
        val param = scopeNode.typeParams?.find {
            it.binder.identifier.name == ident.name
        }
        if (param != null) TypeBinding.TypeParam(param.binder) else null
    }
    is Declaration.TraitDef -> {
        if (scopeNode.name.identifier.name == ident.name) {
            TypeBinding.Trait(scopeNode)
        }

        val associatedType = scopeNode.members.filterIsInstance<Declaration.TraitMember.AssociatedType>()
            .find { it.binder.name == ident.name }
        if (associatedType != null) {
            TypeBinding.AssociatedType(associatedType.binder)
        } else {
            scopeNode.params.find {
                it.binder.identifier.name == ident.name
            }?.let { TypeBinding.TypeParam(it.binder) }
        }
    }
    is Declaration.ImplementationDef -> {
        val aliasDef = scopeNode.body.filterIsInstance<Declaration.TypeAlias>().find {
            it.name.name == ident.name
        }
        if (aliasDef != null) {
            TypeBinding.TypeAlias(aliasDef)
        } else {
            scopeNode.typeParams?.find {
                it.binder.identifier.name == ident.name
            }?.let { TypeBinding.TypeParam(it.binder) }
        }
    }
    is Expression.Closure -> null
    is Expression.Match -> null
    is Expression.Match.Arm -> null
    is Statement.While -> null
}

/**
 * Only consider direct bindings visible in [sourceFile], not children
 */
internal fun findTypeInSourceFile(ctx: SourceFileResolverCtx, ident: Identifier, sourceFile: SourceFile): TypeBinding? {
    for (declaration in sourceFile.declarations) {
        val binding = if (declaration is Declaration.Struct && declaration.binder.identifier.name == ident.name) {
            TypeBinding.Struct(declaration)
        } else if (declaration is Declaration.TypeAlias && declaration.name.identifier.name == ident.name) {
            TypeBinding.TypeAlias(declaration)
        } else if (declaration is Declaration.TraitDef && declaration.name.identifier.name == ident.name) {
            TypeBinding.Trait(declaration)
        } else if (declaration is Declaration.Enum && declaration.name.identifier.name == ident.name) {
            TypeBinding.Enum(declaration)
        } else if (declaration is Declaration.ImportMembers) {
            val binding = declaration.names.find { it.name == ident.name }
            val importedSourceFile = ctx.resolveSourceFile(declaration.modulePath)
            if (importedSourceFile != null && binding != null) {
                findTypeInSourceFile(ctx, ident, importedSourceFile)
            } else {
                null
            }
        } else {
            null
        }
        if (binding != null) {
            return binding
        }
    }
    return null
}

private fun findTypeInFunctionDef(ident: Identifier, declaration: Declaration.FunctionDef): TypeBinding? {
    val typeParams = declaration.typeParams ?: return null
    typeParams.forEach {
        if (it.binder.identifier.name == ident.name) {
            return TypeBinding.TypeParam(it.binder)
        }
    }
    return null
}
