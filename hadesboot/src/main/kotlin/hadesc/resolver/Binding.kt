package hadesc.resolver

import hadesc.ast.Binder
import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.Statement
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class Binding {
    abstract val binder: Binder
    data class GlobalFunction(
        val declaration: Declaration.FunctionDef
    ) : Binding() {
        override val binder: Binder
            get() = declaration.name
    }

    data class ExternFunction(
            val declaration: Declaration.ExternFunctionDef
    ) : Binding() {
        override val binder: Binder get() = declaration.binder
    }

    data class FunctionParam(
            val index: Int,
            val declaration: Declaration.FunctionDef
    ) : Binding() {
        val param get() = declaration.params[index]
        override val binder: Binder
            get() = param.binder
    }

    data class ValBinding(
            val statement: Statement.Val
    ) : Binding() {
        override val binder get() = statement.binder
    }

    data class Struct(
            val declaration: Declaration.Struct
    ) : Binding() {
        override val binder get() = declaration.binder
    }

    data class GlobalConst(
            val declaration: Declaration.ConstDefinition
    ) : Binding() {
        override val binder get() = declaration.name
    }

    data class ClosureParam(val index: Int, val closure: Expression.Closure) : Binding() {
        val param get() = closure.params[index]

        override val binder get() = param.binder
    }

    data class SealedType(val declaration: Declaration.SealedType) : Binding() {
        override val binder: Binder get() = declaration.name
    }

    data class WhenArm(override val binder: Binder, val case: Expression.WhenArm): Binding()

    fun isLocalTo(scope: HasLocation) = binder.location.isWithin(scope.location)

    fun isGlobal() = when(this) {
        is ClosureParam -> false
        is ExternFunction -> true
        is FunctionParam -> false
        is GlobalConst -> true
        is GlobalFunction -> true
        is SealedType -> true
        is Struct -> true
        is ValBinding -> false
        is WhenArm -> false
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