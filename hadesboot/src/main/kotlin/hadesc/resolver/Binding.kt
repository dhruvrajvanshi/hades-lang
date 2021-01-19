package hadesc.resolver

import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.Statement

sealed class Binding {
    data class GlobalFunction(
            val declaration: Declaration.FunctionDef
    ) : Binding()

    data class ExternFunction(
            val declaration: Declaration.ExternFunctionDef
    ) : Binding()

    data class FunctionParam(
            val index: Int,
            val declaration: Declaration.FunctionDef
    ) : Binding() {
        val param get() = declaration.params[index]
    }

    data class WhereParam(
            val index: Int,
            val declaration: WhereBindingDeclaration
    ) : Binding() {
        val traitRequirement get() = declaration.traitRequirements[index]
    }

    data class ValBinding(
            val statement: Statement.Val
    ) : Binding()

    data class Struct(
            val declaration: Declaration.Struct
    ) : Binding()

    data class GlobalConst(
            val declaration: Declaration.ConstDefinition
    ) : Binding()

    data class ClosureParam(val index: Int, val closure: Expression.Closure) : Binding() {
        val param get() = closure.params[index]
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