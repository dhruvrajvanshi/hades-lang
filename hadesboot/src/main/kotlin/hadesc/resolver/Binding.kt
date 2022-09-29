package hadesc.resolver

import hadesc.ast.*
import hadesc.location.HasLocation

sealed interface Binding {
    val binder: Binder
    sealed interface Local: Binding

    data class GlobalFunction(
        val declaration: Declaration.FunctionDef
    ) : Binding {
        override val binder: Binder
            get() = declaration.name
    }

    data class ExternFunction(
            val declaration: Declaration.ExternFunctionDef
    ) : Binding {
        override val binder: Binder get() = declaration.binder
    }

    data class ExternConst(
        val declaration: Declaration.ExternConst
    ) : Binding {
        override val binder: Binder get() = declaration.name
    }

    data class FunctionParam(
            val index: Int,
            val declaration: Declaration.FunctionDef
    ) : Binding, Local {
        val param get() = declaration.params[index]
        override val binder: Binder
            get() = param.binder
    }

    data class ValBinding(
            val statement: Statement.Val
    ) : Binding, Local {
        override val binder get() = statement.binder
    }

    data class Struct(
            val declaration: Declaration.Struct
    ) : Binding {
        override val binder get() = declaration.binder
    }

    data class GlobalConst(
            val declaration: Declaration.ConstDefinition
    ) : Binding {
        override val binder get() = declaration.name
    }

    data class ClosureParam(val index: Int, val closure: Expression.Closure) : Binding, Local {
        val param get() = closure.params[index]

        override val binder get() = param.binder
    }

    data class Enum(val declaration: Declaration.Enum) : Binding {
        override val binder: Binder get() = declaration.name
    }

    data class MatchArmEnumCaseArg(val topLevelPattern: Pattern.EnumCase, val argIndex: Int): Binding, Local {
        init {
            requireNotNull(topLevelPattern.args)
            require(argIndex < topLevelPattern.args.size)
            require(topLevelPattern.args[argIndex] is Pattern.Val)
        }

        override val binder: Binder
            get() = arg.binder

        val arg get(): Pattern.Val = checkNotNull(topLevelPattern.args)[argIndex] as Pattern.Val
    }

    fun isLocalTo(scope: HasLocation) = binder.location.isWithin(scope.location)

    fun isGlobal() = when(this) {
        is ClosureParam -> false
        is ExternFunction -> true
        is FunctionParam -> false
        is GlobalConst -> true
        is GlobalFunction -> true
        is Enum -> true
        is Struct -> true
        is ValBinding -> false
        is ExternConst -> true
        is MatchArmEnumCaseArg -> false
    }
}

sealed class WhereBindingDeclaration {
    data class FunctionDef(val declaration: Declaration.FunctionDef) : WhereBindingDeclaration()
    data class ImplementationDef(val declaration: Declaration.ImplementationDef) : WhereBindingDeclaration()

    val traitRequirements get() = when(this) {
        is FunctionDef -> declaration.signature.whereClause?.traitRequirements ?: listOf()
        is ImplementationDef -> declaration.whereClause?.traitRequirements ?: listOf()
    }
}