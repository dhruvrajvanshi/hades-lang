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
    data class Enum(val declaration: Declaration.Enum) : ScopeTree()
    data class MatchArm(val arm: Expression.Match.Arm) : ScopeTree()
    data class TypeAlias(val declaration: Declaration.TypeAlias) : ScopeTree()
    data class ExtensionDef(val declaration: Declaration.ExtensionDef) : ScopeTree()

    val location
        get(): SourceLocation = when (this) {
            is FunctionDef -> declaration.location
            is SourceFile -> sourceFile.location
            is Block -> block.location
            is Struct -> declaration.location
            is Enum -> declaration.location
            is MatchArm -> arm.location
            is TypeAlias -> declaration.location
            is ExtensionDef -> declaration.location
        }
}