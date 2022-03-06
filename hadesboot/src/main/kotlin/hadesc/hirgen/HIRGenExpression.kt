package hadesc.hirgen

import hadesc.assertions.requireUnreachable
import hadesc.ast.Expression
import hadesc.context.Context
import hadesc.hir.HIRExpression
import hadesc.resolver.Binding

internal class HIRGenExpression(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext
): HIRGenModuleContext by moduleContext {
    internal fun lowerVarExpression(expression: Expression.Var): HIRExpression {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> requireUnreachable {
                "Found unresolved variable: ${expression.name} at ${expression.location}"
            }
            else -> lowerBinding(expression, binding)
        }
    }


    internal fun lowerBinding(
        expression: Expression,
        binding: Binding
    ): HIRExpression = when(binding) {
        is Binding.GlobalFunction -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.ExternFunction -> {
            getExternDef(binding.declaration)
            HIRExpression.GlobalRef(
                expression.location,
                typeOfExpression(expression),
                lowerGlobalName(binding.declaration.binder)
            )
        }
        is Binding.FunctionParam -> HIRExpression.ParamRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.param.binder),
            binding.binder
        )
        is Binding.ValBinding -> HIRExpression.ValRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.statement.binder)
        )
        is Binding.Struct -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.binder)
        )
        is Binding.GlobalConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.ClosureParam -> HIRExpression.ParamRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.param.binder),
            binding.binder,
        )
        is Binding.Enum -> TODO()
        is Binding.ExternConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.MatchArmEnumCaseArg -> HIRExpression.ValRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.arg.binder)
        )
    }


}