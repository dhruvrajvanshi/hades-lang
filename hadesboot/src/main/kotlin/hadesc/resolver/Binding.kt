package hadesc.resolver

import hadesc.ast.Declaration
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

    data class ValBinding(
            val statement: Statement.Val
    ) : Binding()

    data class Struct(
            val declaration: Declaration.Struct
    ) : Binding()

    data class GlobalConst(
            val declaration: Declaration.ConstDefinition
    ) : Binding()

    data class EnumCaseConstructor(
            val declaration: Declaration.Enum,
            val caseIndex: Int
    ) : Binding() {
        val case get() = declaration.cases[caseIndex]
    }

    data class Pattern(val pattern: hadesc.ast.Pattern.Name) : Binding()
}