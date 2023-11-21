package hadesc.analysis

import hadesc.ast.Expression
import hadesc.ast.MutableNodeMap
import hadesc.types.Type

interface TyCtx {
    fun typeOfExpression(expr: Expression): Type
    val Expression.type: Type get() = typeOfExpression(this)
}
interface MutableTyCtx: TyCtx {
    fun setTypeOfExpression(expr: Expression, type: Type)

    companion object {
        @JvmStatic
        fun make(): MutableTyCtx = TyCtxImpl()
    }
}

private class TyCtxImpl(): MutableTyCtx {
    private val exprTypes = MutableNodeMap<Expression, Type>()
    private val annotationTypes = MutableNodeMap<Expression, Type>()
    override fun typeOfExpression(expr: Expression): Type = requireNotNull(exprTypes[expr])

    override fun setTypeOfExpression(expr: Expression, type: Type) {
        check(exprTypes[expr] == null)
        exprTypes[expr] = type
    }
}