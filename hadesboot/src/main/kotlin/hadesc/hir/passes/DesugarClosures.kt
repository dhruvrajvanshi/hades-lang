package hadesc.hir.passes

import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.hir.HIRStatement.AllocateClosure
import hadesc.hir.HIRStatement.InvokeClosure
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.types.Type
import hadesc.types.ptr
import java.nio.file.Path

class DesugarClosures(override val namingCtx: NamingContext): AbstractHIRTransformer() {

    override fun transformAllocateClosure(statement: AllocateClosure): Collection<HIRStatement> {
        val closureRef = emitAlloca(statement.name, closureStruct.instanceType(listOf(statement.type.to)))
        emitStore(
            closureRef.mutPtr().fieldPtr(closureFunctionPtrName),
            statement.function.ptrCast(
                fnTypeThatReturns(statement.type.to)
            )
        )
        emitStore(
            closureRef.mutPtr().fieldPtr(closureCtxFieldName),
            transformOperand(statement.ctxPtr).ptrCast(Type.Void)
        )
        return emptyList()
    }

    override fun transformInvokeClosureStatement(statement: InvokeClosure): Collection<HIRStatement> {
        val closureRef = transformExpression(statement.closureRef)
        check(closureRef.type is Type.Ptr)
        val fnPtr = closureRef.fieldPtr(closureFunctionPtrName).load().ptrCast(
            toPointerOfType = Type.Function(
                from = statement.args.map { it.type } + Type.Void.ptr(),
                to = statement.type
            )
        )
        val ctxPtr = closureRef.fieldPtr(closureCtxFieldName).load()

        check(ctxPtr.type == Type.Void.ptr())

        emitCall(
            statement.type,
            fnPtr,
            statement.args.map { transformExpression(it) } + ctxPtr,
            name = statement.name
        )
        return emptyList()
    }

    override fun lowerType(type: Type): Type {
        return when (type) {
            is Type.Ptr ->
                if (type.to is Type.Function) {
                    return Type.Ptr(
                        to = Type.Function(
                            from = type.to.from.map { lowerType(it) },
                            to = lowerType(type.to.to)
                        ),
                        isMutable = false
                    )
                } else {
                    super.lowerType(type)
                }
            is Type.Function ->
                closureStruct.instanceType(listOf(type.to)).ptr()
            else -> super.lowerType(type)
        }
    }

    private val closureCtxFieldName = namingCtx.makeName("ctx")
    private val closureFunctionPtrName = namingCtx.makeName("fn")
    private val closureStruct by lazy {
        val location = SourceLocation(
            SourcePath(Path.of("builtin.Closure")),
            Position(0, 0),
            Position(0, 0)
        )
        val structName = namingCtx.makeName("\$builtin.Closure")
        val typeParamName = namingCtx.makeName("T")
        val typeParam = Binder(Identifier(location, typeParamName))
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = listOf(HIRTypeParam(location, typeParamName)),
            fields = listOf(
                closureCtxFieldName to Type.Void.ptr(),
                closureFunctionPtrName to fnTypeThatReturns(Type.ParamRef(typeParam)).ptr(),
            ),
            name = structName.toQualifiedName()
        )
        currentModule.addDefinition(def)
        def

    }

    private fun fnTypeThatReturns(returns: Type): Type.Function {
        return Type.Function(
            from = listOf(),
            to = returns
        )
    }
}
