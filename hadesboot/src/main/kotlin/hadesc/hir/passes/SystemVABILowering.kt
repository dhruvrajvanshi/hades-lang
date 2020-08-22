package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.types.Type

/**
 * This transformation pass converts functions and calls
 * to System-V ABI compatible instructions/definitions.
 * This involves things like passing "medium sized" structs
 * as multiple arguments, large structs by value, etc.
 * This ensures that the compiled code is interoperable with
 * separately compiled C libraries.
 */
@OptIn(ExperimentalStdlibApi::class)
class SystemVABILowering(val oldModule: HIRModule, val ctx: Context): HIRTransformer {

    override fun transformExternFunctionDef(definition: HIRDefinition.ExternFunction): Collection<HIRDefinition> {
        val returnType = if (isTypePassedByPointer(definition.returnType)) {
            Type.Void
        } else definition.returnType
        val params = buildList {
            if (isTypePassedByPointer(definition.returnType)) {
                add(Type.Ptr(lowerType(definition.returnType), isMutable = true))
            }
            for (originalParamType in definition.params) {
                if (isTypePassedByPointer(originalParamType)) {
                    add(Type.Ptr(lowerType(originalParamType), isMutable = false))
                } else {
                    add(lowerType(originalParamType))
                }
            }
        }
        return listOf(
                HIRDefinition.ExternFunction(
                        definition.location,
                        definition.name,
                        params,
                        returnType,
                        definition.externName
                )
        )
    }

    private fun addStatement(statement: HIRStatement) {
        requireNotNull(currentStatements).add(statement)
    }

    override fun transformCall(expression: HIRExpression.Call): HIRExpression {
        if (expression.callee !is HIRExpression.GlobalRef) {
            return super.transformCall(expression)
        }
        if (oldModule.findGlobalDefinition(expression.callee.name) is HIRDefinition.Struct) {
            return super.transformCall(expression)
        }
        val returnName = ctx.makeUniqueName()
        if (isTypePassedByPointer(expression.type)) {
            addStatement(HIRStatement.ValDeclaration(
                    location = expression.location,
                    name = returnName,
                    isMutable = true,
                    type = lowerType(expression.type)))
        }
        val args = buildList {
            if (isTypePassedByPointer(expression.type)) {
                add(HIRExpression.AddressOf(expression.location, Type.Ptr(lowerType(expression.type), isMutable = true), returnName))
            }
            for (arg in expression.args) {
                if (isTypePassedByPointer(arg.type)) {
                    val tempArgName = ctx.makeUniqueName()
                    addStatement(HIRStatement.ValDeclaration(
                            arg.location,
                            tempArgName,
                            type = lowerType(arg.type),
                            isMutable = false))
                    addStatement(HIRStatement.Assignment(arg.location, tempArgName, transformExpression(arg)))
                    add(HIRExpression.AddressOf(arg.location, Type.Ptr(lowerType(arg.type), isMutable = false), tempArgName))
                } else {
                    add(transformExpression(arg))
                }
            }
        }
        return if (isTypePassedByPointer(expression.type)) {
            addStatement(HIRStatement.Expression(
                    HIRExpression.Call(
                            expression.location,
                            Type.Void,
                            transformExpression(expression.callee),
                            args
                    )
            ))
            HIRExpression.ValRef(expression.location, expression.type, returnName)
        } else {
            HIRExpression.Call(
                    expression.location,
                    lowerType(expression.type),
                    transformExpression(expression.callee),
                    args
            )
        }
    }

    private var currentStatements : MutableList<HIRStatement>? = null
    override fun transformBlock(body: HIRBlock): HIRBlock {
        val oldStatements = currentStatements
        currentStatements = mutableListOf()
        for (statement in body.statements) {
            requireNotNull(currentStatements).addAll(transformStatement(statement))
        }
        val statements = requireNotNull(currentStatements)
        currentStatements = oldStatements
        return HIRBlock(
                location = body.location,
                statements = statements
        )
    }

    private var indirectReturnPtr: HIRExpression? = null
    private var indirectParams: MutableMap<Name, HIRParam> = mutableMapOf()
    override fun transformFunctionDef(definition: HIRDefinition.Function): Collection<HIRDefinition> {
        val loweredReturnType = lowerType(definition.returnType)
        val isReturnTypePassedByPointer = isTypePassedByPointer(loweredReturnType)
        val returnType = if (isReturnTypePassedByPointer) {
            Type.Void
        } else {
            loweredReturnType
        }
        val outParamList = mutableListOf<HIRParam>()
        if (isReturnTypePassedByPointer) {
            val outParam = HIRParam(
                    location = definition.location,
                    type = Type.Ptr(loweredReturnType, isMutable = true),
                    name = ctx.makeUniqueName())
            outParamList.add(outParam)
            indirectReturnPtr = HIRExpression.ParamRef(
                    definition.location,
                    outParam.type,
                    outParam.name)
        }
        val params = buildList {
            addAll(outParamList)
            val indirectParamsValue = mutableMapOf<Name, HIRParam>()
            for (param in definition.params) {
                if (isTypePassedByPointer(param.type)) {
                    val indirectParam = HIRParam(
                            location = param.location,
                            type = Type.Ptr(param.type, isMutable = false),
                            name = ctx.makeUniqueName()
                    )
                    indirectParamsValue[param.name] = indirectParam
                    add(indirectParam)
                } else {
                    add(transformParam(param))
                }
            }
            indirectParams = indirectParamsValue
        }
        val body = transformBlock(definition.body)
        indirectReturnPtr = null
        require(params.size == outParamList.size + definition.params.size)
        return listOf(
                HIRDefinition.Function(
                        location = definition.location,
                        signature = HIRFunctionSignature(
                                definition.signature.location,
                                name = definition.name,
                                constraintParams = null,
                                params = params,
                                returnType = returnType,
                                typeParams = null
                        ),
                        body = body
                )
        )
    }

    override fun transformParamRef(expression: HIRExpression.ParamRef): HIRExpression {
        val indirectPtr = indirectParams[expression.name]
        if (indirectPtr != null) {
            return HIRExpression.Load(
                    expression.location,
                    expression.type,
                    HIRExpression.ParamRef(expression.location, expression.type, indirectPtr.name)
            )
        }
        return super.transformParamRef(expression)
    }

    override fun transformReturnStatement(statement: HIRStatement.Return): Collection<HIRStatement> {
        val outParam = indirectReturnPtr
        if (outParam != null) {
            return listOf(
                    HIRStatement.Store(statement.location, ptr = outParam, value = transformExpression(statement.expression)),
                    HIRStatement.ReturnVoid(statement.location)
            )
        }
        return super.transformReturnStatement(statement)
    }

    private fun isTypePassedByPointer(type: Type): Boolean {
        return abiSizeBytes(type) > 16
    }

    private fun getTypeClass(type: Type): TypeClass = when(type) {
        is Type.Ptr -> TypeClass.POINTER
        Type.Bool -> TypeClass.INTEGER
        Type.CInt -> TypeClass.INTEGER
        Type.Size -> TypeClass.INTEGER
        is Type.Integral -> TypeClass.INTEGER
        Type.Byte -> TypeClass.INTEGER

        is Type.FloatingPoint -> TypeClass.SSE
        Type.Double -> TypeClass.SSE

        Type.Void -> TypeClass.NO_CLASS
        is Type.Constructor -> TODO()
        is Type.UntaggedUnion -> TODO()

        Type.Error -> requireUnreachable()
        is Type.Function -> requireUnreachable()
        is Type.ParamRef -> requireUnreachable()
        is Type.TypeFunction -> requireUnreachable()
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> requireUnreachable()
    }

    override fun lowerFunctionType(type: Type.Function): Type {
        val from = buildList {
            if (isTypePassedByPointer(type.to)) {
                add(Type.Ptr(lowerType(type.to), isMutable = true))
            }
            for (param in type.from) {
                if (isTypePassedByPointer(param))  {
                    add(Type.Ptr(lowerType(param), isMutable = false))
                } else {
                    add(lowerType(param))
                }
            }
        }
        val to = if (isTypePassedByPointer(type.to)) {
            Type.Void
        } else {
            lowerType(type.to)
        }
        val result = Type.Function(
                from,
                to
        )
        return result
    }

    private fun abiSizeBytes(type: Type): Int = when (type) {
        Type.Byte -> 1
        Type.Void -> 0
        Type.Bool -> 1
        Type.CInt -> 4
        is Type.Ptr -> 8
        is Type.Integral -> type.size / 8
        is Type.FloatingPoint -> type.size / 8
        Type.Double -> 8
        Type.Size -> 8

        is Type.UntaggedUnion -> {
            type.members.map { abiSizeBytes(it) }.maxOrNull() ?: 0
        }
        is Type.Constructor -> {
            val def = oldModule.findGlobalDefinition(type.name)
            require(def is HIRDefinition.Struct)
            if (def.fields.isEmpty()) {
                0
            } else {
                def.fields
                        .map { abiSizeBytes(it.second) }
                        .reduce { a, b -> a + b }
            }
        }

        is Type.Function -> requireUnreachable()
        is Type.ParamRef -> requireUnreachable()
        is Type.TypeFunction -> requireUnreachable()
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> requireUnreachable()
        Type.Error -> requireUnreachable()
    }

}

enum class TypeClass {
    POINTER,
    /**
     * integral types (other than pointers) that fit in 1 register
     */
    INTEGER,
    /**
     * types that fit into vector registers
     */
    SSE,
    /**
     * The class consists of types that fit into a vector register and can be passedand returned in the upper bytes of it.
     */
    SSEUP,
    /**
     * These  classes  consists  of  types  that  will  be  returned  via  the  x87FPU
     */
    X87, X87UP,

    /**
     * This class consists of types that will be returned via the x87FPU
     */
    COMPLEX_X87,

    /**
     * This class is used as initializer in the algorithms. It will be used for
     * padding and empty structures and unions.
     */
    NO_CLASS,

    /**
     * This class consists of types that will be passed and returned in memory via the stack.
     */
    MEMORY,
}