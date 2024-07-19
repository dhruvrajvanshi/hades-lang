package hadesc.hir.passes

import hadesc.context.IdGenCtx
import hadesc.context.NamingCtx
import hadesc.hir.*
import hadesc.hir.HIRStatement.AllocateClosure
import hadesc.hir.HIRStatement.InvokeClosure
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr
import java.nio.file.Path

class DesugarClosures(
    override val namingCtx: NamingCtx,
    private val idCtx: IdGenCtx,
) : AbstractHIRTransformer() {

    override fun transformAllocateClosure(statement: AllocateClosure): Collection<HIRStatement> {
        val closureRef = emitAlloca(statement.name, closureStruct.instanceType())
        emitStore(
            closureRef.mutPtr().fieldPtr(closureFunctionPtrName),
            statement.function.ptrCast(Type.Void)
        )
        emitStore(
            closureRef.mutPtr().fieldPtr(closureCtxFieldName),
            transformOperand(statement.ctxPtr).ptrCast(Type.Void)
        )
        return emptyList()
    }

    override fun transformInvokeClosureStatement(statement: InvokeClosure): Collection<HIRStatement> {
        val closureRef = transformExpression(statement.closureRef)
        val fnPtr = closureRef.fieldPtr(closureFunctionPtrName).ptrCast(
            Type.FunctionPtr(
                from = statement.args.map { it.type } + Type.Void.ptr(),
                to = statement.type
            )
        ).load()
        val ctxPtr = closureRef.fieldPtr(closureCtxFieldName).load()

        check(ctxPtr.type == Type.Void.mutPtr())

        emitCall(
            fnPtr,
            statement.args.map { transformExpression(it) } + ctxPtr,
            name = statement.name
        )
        return emptyList()
    }

    override fun lowerType(type: Type): Type {
        return when (type) {
            is Type.Closure ->
                closureStruct.instanceType().ptr()
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
        val def = HIRDefinition.Struct(
            location = location,
            typeParams = null,
            fields = listOf(
                closureCtxFieldName to Type.Void.mutPtr(),
                closureFunctionPtrName to Type.Void.ptr(),
            ),
            name = structName.toQualifiedName()
        )
        currentModule.addDefinition(def)
        def
    }

    private fun fnTypeThatReturns(returns: Type): Type.FunctionPtr {
        return Type.FunctionPtr(
            from = listOf(),
            to = returns
        )
    }
}
