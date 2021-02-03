package hadesc.resolver

import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.location.SourceLocation

sealed class ScopeTree {
    data class FunctionDef(
            val declaration: Declaration.FunctionDef
    ) : ScopeTree()

    data class SourceFile(
            val sourceFile: hadesc.ast.SourceFile
    ) : ScopeTree()

    data class Block(val block: hadesc.ast.Block) : ScopeTree()
    data class Struct(val declaration: Declaration.Struct) : ScopeTree()
    data class TypeAlias(val declaration: Declaration.TypeAlias) : ScopeTree()
    data class ExtensionDef(val declaration: Declaration.ExtensionDef) : ScopeTree()
    data class TraitDef(val declaration: Declaration.TraitDef) : ScopeTree()
    data class ImplementationDef(val declaration: Declaration.ImplementationDef) : ScopeTree()
    data class Closure(val closure: Expression.Closure) : ScopeTree()
    data class SealedTypeDef(val declaration: Declaration.SealedType) : ScopeTree()
    data class WhenArm(val whenArm: Expression.WhenArm) : ScopeTree()
    data class WhenExpression(val expression: Expression.When) : ScopeTree()

    val location
        get(): SourceLocation = when (this) {
            is FunctionDef -> declaration.location
            is SourceFile -> sourceFile.location
            is Block -> block.location
            is Struct -> declaration.location
            is TypeAlias -> declaration.location
            is ExtensionDef -> declaration.location
            is TraitDef -> declaration.location
            is ImplementationDef -> declaration.location
            is Closure -> closure.location
            is SealedTypeDef -> declaration.location
            is WhenArm -> whenArm.value.location
            is WhenExpression -> expression.location
        }
}