package hadesc.resolver

import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.location.SourceLocation

sealed class ScopeNode {
    data class FunctionDef(
            val declaration: Declaration.FunctionDef
    ) : ScopeNode()

    data class SourceFile(
            val sourceFile: hadesc.ast.SourceFile
    ) : ScopeNode()

    data class Block(val block: hadesc.ast.Block) : ScopeNode()
    data class Struct(val declaration: Declaration.Struct) : ScopeNode()
    data class Enum(val declaration: Declaration.Enum) : ScopeNode()
    data class MatchArm(val arm: Expression.Match.Arm) : ScopeNode()
    data class Interface(val declaration: Declaration.Interface) : ScopeNode()
    data class Implementation(val declaration: Declaration.Implementation) : ScopeNode()
    data class TypeAlias(val declaration: Declaration.TypeAlias) : ScopeNode()

    val location
        get(): SourceLocation = when (this) {
            is FunctionDef -> declaration.location
            is SourceFile -> sourceFile.location
            is Block -> block.location
            is Struct -> declaration.location
            is Interface -> declaration.location
            is Enum -> declaration.location
            is MatchArm -> arm.location
            is TypeAlias -> declaration.location
            is Implementation -> declaration.location
        }
}