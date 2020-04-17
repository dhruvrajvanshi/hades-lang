package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRBinding {
    data class FunctionDef(val def: IRFunctionDef) : IRBinding()
    data class ExternFunctionDef(val def: IRExternFunctionDef) : IRBinding()
    data class ValStatement(val statement: IRValStatement) : IRBinding()
    data class StructDef(val def: IRStructDef) : IRBinding()
    data class ParamRef(val def: IRFunctionDef, val index: Int) : IRBinding()
}

data class IRModule(
    val definitions: List<IRDefinition>
)

sealed class IRDefinition
data class IRFunctionDef(
    val binder: IRBinder,
    val typeParams: List<IRTypeBinder>,
    val params: List<IRParam>,
    val body: IRBlock
) : IRDefinition()

data class IRStructDef(
    val globalName: Name,
    val typeParams: List<IRTypeBinder>,
    val fields: Map<Name, Type>
) : IRDefinition()

data class IRExternFunctionDef(
    val binder: IRBinder,
    val paramTypes: List<Type>,
    val externName: Name
) : IRDefinition()

data class IRBinder(val name: Name, val type: Type)
inline class IRParam(val binder: IRBinder)

data class IRTypeBinder(val name: Name)

data class IRBlock(val statements: List<IRStatement>)

sealed class IRStatement
data class IRValStatement(val binder: IRBinder, val initializer: IRExpression) : IRStatement()
data class IRReturnStatement(val value: IRExpression) : IRStatement()

sealed class IRExpression : IRStatement() {
    abstract val type: Type
    abstract val location: SourceLocation
}

data class IRCallExpression(
    override val type: Type,
    override val location: SourceLocation,
    val callee: IRExpression,
    val typeArgs: List<Type>?,
    val args: List<IRExpression>
) : IRExpression()

data class IRBool(
    override val type: Type,
    override val location: SourceLocation,
    val value: Boolean
) : IRExpression()

data class IRByteString(
    override val type: Type,
    override val location: SourceLocation,
    val value: ByteArray
) : IRExpression()

data class IRVariable(
    override val type: Type,
    override val location: SourceLocation,
    val binding: IRBinding
) : IRExpression()

data class IRGetStructField(
    override val type: Type,
    override val location: SourceLocation,
    val lhs: IRExpression,
    val rhs: Name
) : IRExpression()


