package hadesc.hir.transformers

import hadesc.Name
import hadesc.ast.Binder
import hadesc.context.Context
import hadesc.hir.HIRExpression
import hadesc.hir.HIRParam
import hadesc.hir.HIRStatement
import hadesc.hir.passes.AbstractHIRTransformer
import hadesc.location.TaggedLocation
import hadesc.location.taggedLocation
import llvm.makeList

/**
 * Since params can be passed directly in registers, it is not possible to
 * take their address directly. This isn't an issue normally, but closure
 * captures are passed in a struct of pointers to each capture.
 *
 * This class hooks into 2 parts of HIRGen
 *
 * [declareParamCopies] returns statements that copy all params into local vals
 *       - def foo(p1, p2) { ...declareParamCopies(p1, p2); ... }
 *         -> def foo(p1, p2) { val p1_copy = p1; val p2_copy = p2; ... }
 *
 * [fixParamRef]: replaces ParamRef variables in with references to their copies created by [declareParamCopies]
 * This doesn't recurse on nested expressions, hence, it must be called in a bottom up way.
 *
 * It is important that this method is called after calling [declareParamCopies]
 * for the current function. Since params are always declared before their usages,
 * this is trivial.
 *
 */
class ParamToLocal(override val namingCtx: Context): AbstractHIRTransformer() {
    private val paramCopies = mutableMapOf<TaggedLocation<Binder>, Name>()

    fun declareParamCopies(params: List<HIRParam>): List<HIRStatement> =
        makeList {
            params.forEach { addAll(declareParamCopy(it)) }
        }

    fun fixBinder(it: Binder): Binder =
        Binder(
            it.identifier.copy(
                name = paramCopies[it.taggedLocation()] ?: it.name
            )
        )

    fun fixParamRef(expression: HIRExpression.ParamRef): HIRExpression =
        if (expression.name.text == "this")
            // TODO: Make param copies for this params
            expression
        else
            HIRExpression.ValRef(
                expression.location,
                expression.type,
                checkNotNull(paramCopies[expression.binder.taggedLocation()])
            )

    private fun declareParamCopy(param: HIRParam) = makeList {
        val copyName = namingCtx.makeName("%${param.name.text}%copy")
        check(paramCopies[param.binder.taggedLocation()] == null) {"${param.location}: Duplicate param: ${param.name.text}"}
        paramCopies[param.binder.taggedLocation()] = copyName
        add(
            HIRStatement.ValDeclaration(
                param.location,
                name = copyName,
                isMutable = false,
                type = param.type
            )
        )
        add(
            HIRStatement.Assignment(
                param.location,
                name = copyName,
                value = HIRExpression.ParamRef(
                    param.location,
                    param.type,
                    param.name,
                    param.binder,
                )
            )
        )
    }

}