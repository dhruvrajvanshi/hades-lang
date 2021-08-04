package hadesc.hir.passes

import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.types.Type

class SimplifyVoidExpressions: AbstractHIRTransformer() {
    override fun transformValDeclaration(statement: HIRStatement.ValDeclaration): Collection<HIRStatement> {
        return if (statement.type is Type.Void) {
            emptyList()
        } else {
            super.transformValDeclaration(statement)
        }
    }

    override fun transformExpressionStatement(statement: HIRStatement.Expression): Collection<HIRStatement> {
        if (statement.expression.type is Type.Void && statement.expression is HIRExpression.ValRef) {
            return emptyList()
        }
        return super.transformExpressionStatement(statement)
    }

    override fun transformAssignmentStatement(statement: HIRStatement.Assignment): Collection<HIRStatement> {
        return if (statement.value.type is Type.Void) {
            if (statement.value is HIRExpression.ValRef) {
                emptyList()
            } else {
                listOf(
                    HIRStatement.Expression(transformExpression(statement.value))
                )
            }
        } else {
            super.transformAssignmentStatement(statement)
        }
    }
}