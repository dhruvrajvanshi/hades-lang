package hadesc.analysis

import hadesc.ast.Expression
import hadesc.types.Type

interface Infer {
    fun inferExpression(expr: Expression): Type
    companion object {

        fun make(): Infer = InferImpl()
    }
}
private class InferImpl: Infer {
    override fun inferExpression(expr: Expression): Type {
        TODO("Not yet implemented")
    }
}