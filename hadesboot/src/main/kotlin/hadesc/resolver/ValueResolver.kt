package hadesc.resolver

import hadesc.Name
import hadesc.ast.*
import hadesc.context.SourceFileResolverCtx

/**
 * Find bindings introduced at the top level of [scope]
 * For example,
 *  def foo(x: T, y: U): Void {
 *      val z = 4
 *      if (bar()) {
 *          val a = 1;
 *          while (true) {
 *              val b = 4;
 *          }
 *      }
 *  }
 *
 *  Running this on foo will yield x and y (not z because it's introduced by a child scope)
 *
 *  Running this on the if block will yield a
 *  - not z, x, y because they're introduced in parent scopes,
 *  - not b because it's introduced in a child scope
 */
internal fun findInScope(ctx: SourceFileResolverCtx, ident: Identifier, scope: ScopeTree): Binding? = when (scope) {
    is Declaration.FunctionDef -> findInFunctionDef(ident, scope)
    is SourceFile -> findInSourceFile(ctx, ident.name, scope)
    is Block -> findInBlock(ident, scope)
    is Declaration.Struct -> null
    is Declaration.TypeAlias -> null
    is Declaration.ExtensionDef -> null
    is Expression.Closure -> findInClosure(ident, scope)
    is Declaration.Enum -> null
    is Expression.Match -> null
    is Expression.Match.Arm -> findInMatchArm(ident, scope)
    is Statement.While -> findInBlock(ident, scope.body)
}


/**
 * Bindings at the top level scope of [sourceFile]
 */
fun findInSourceFile(ctx: SourceFileResolverCtx, name: Name, sourceFile: SourceFile): Binding? {
    for (declaration in sourceFile.declarations) {
        val binding = when (declaration) {
            is Declaration.Error -> null
            is Declaration.ImportAs -> null
            is Declaration.FunctionDef -> {
                if (declaration.name.identifier.name == name) {
                    Binding.GlobalFunction(declaration)
                } else {
                    null
                }
            }
            is Declaration.ExternFunctionDef -> {
                if (declaration.binder.identifier.name == name) {
                    Binding.ExternFunction(declaration)
                } else {
                    null
                }
            }
            is Declaration.Struct -> {
                if (declaration.binder.identifier.name == name) {
                    Binding.Struct(declaration)
                } else {
                    null
                }
            }
            is Declaration.ConstDefinition -> {
                if (declaration.name.identifier.name == name) {
                    Binding.GlobalConst(declaration)
                } else {
                    null
                }
            }
            is Declaration.ExternConst -> {
                if (declaration.name.identifier.name == name) {
                    Binding.ExternConst(declaration)
                } else {
                    null
                }
            }
            is Declaration.TypeAlias -> null
            is Declaration.ExtensionDef -> null
            is Declaration.ImportMembers -> {
                val importedSourceFile = ctx.resolveSourceFile(declaration.modulePath)
                if (importedSourceFile != null && declaration.names.any { it.name == name }) {
                    findInSourceFile(ctx, name, importedSourceFile)
                } else {
                    null
                }
            }
            is Declaration.Enum -> if (name == declaration.name.identifier.name) {
                Binding.Enum(declaration)
            } else {
                null
            }
        }
        if (binding != null) {
            return binding
        }
    }
    return null
}

private fun findInMatchArm(ident: Identifier, scope: Expression.Match.Arm): Binding? {
    return findInPattern(ident, scope.pattern)
}

private fun findInPattern(ident: Identifier, pattern: Pattern): Binding? = when (pattern) {
    is Pattern.EnumCase ->
        pattern.args
            ?.asReversed() // last match wins
            ?.mapIndexedNotNull { indexFromBack, arg ->
                val index = pattern.args.size - 1 - indexFromBack
                when (arg) {
                    is Pattern.Val -> if (arg.binder.name == ident.name) {
                        Binding.MatchArmEnumCaseArg(pattern, index)
                    } else {
                        null
                    }
                    else -> null
                }
            }
            ?.firstOrNull()
    is Pattern.IntLiteral -> null
    is Pattern.Val -> TODO()
    is Pattern.Wildcard -> null
}
private fun findInBlock(ident: Identifier, scope: Block): Binding? {
    for (member in scope.members) {
        val binding = when (member) {
            is Block.Member.Expression -> null
            is Block.Member.Statement -> when (member.statement) {
                is Statement.Return -> null
                is Statement.Val -> if (ident.name == member.statement.binder.identifier.name) {
                    Binding.ValBinding(member.statement)
                } else {
                    null
                }
                is Statement.Error -> null
                is Statement.While -> null
                is Statement.If -> null
                is Statement.LocalAssignment -> null
                is Statement.MemberAssignment -> null
                is Statement.PointerAssignment -> null
                is Statement.Defer -> null
            }
        }
        if (binding != null) {
            return binding
        }
    }
    return null
}
private fun findInClosure(ident: Identifier, scope: Expression.Closure): Binding? {
    var index = -1
    for (param in scope.params) {
        index++
        if (param.binder.identifier.name == ident.name) {
            return Binding.ClosureParam(index, scope)
        }
    }
    return null
}

private fun findInFunctionDef(ident: Identifier, scope: Declaration.FunctionDef): Binding? {
    var index = -1
    for (param in scope.params) {
        index++
        if (param.binder.identifier.name == ident.name) {
            return Binding.FunctionParam(index, scope)
        }
    }

    return if (
        scope.typeParams == null &&
        ident.name == scope.name.identifier.name
    ) {
        Binding.GlobalFunction(scope)
    } else {
        null
    }
}

