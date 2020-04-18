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
) {
    fun prettyPrint(): String = definitions.joinToString("\n") { it.prettyPrint() }
}

sealed class IRDefinition {
    fun prettyPrint(): String = when (this) {
        is IRFunctionDef -> "def ${binder.prettyPrint()} = (${params.joinToString(", ") { it.prettyPrint() }}) ${body.prettyPrint()}"
        is IRStructDef -> "struct ${this.globalName.text} {" +
                "\n${fields.entries.joinToString("\n") { "  val ${it.key.text}: ${it.value.prettyPrint()};" }}\n}"
        is IRExternFunctionDef -> "extern def ${binder.prettyPrint()} = ${externName.text}"
    }
}

data class IRFunctionDef(
    val binder: IRBinder,
    val typeParams: List<IRTypeBinder>?,
    val params: List<IRParam>,
    var body: IRBlock
) : IRDefinition() {
    val type get() = binder.type
}

data class IRStructDef(
    val constructorType: Type,
    val instanceType: Type,
    val globalName: Name,
    val typeParams: List<IRTypeBinder>,
    val fields: Map<Name, Type>
) : IRDefinition()

data class IRExternFunctionDef(
    val binder: IRBinder,
    val paramTypes: List<Type>,
    val externName: Name
) : IRDefinition() {
    val type get() = binder.type
}

data class IRBinder(val name: Name, val type: Type) {
    fun prettyPrint(): String = "${name.text} : ${type.prettyPrint()}"
}

inline class IRParam(val binder: IRBinder) {
    fun prettyPrint(): String = "${binder.name.text} : ${binder.type.prettyPrint()}"
}

data class IRTypeBinder(val name: Name)

data class IRBlock(val statements: MutableList<IRStatement>) {
    fun prettyPrint(): String = "{\n${statements.joinToString("\n") { "  " + it.prettyPrint() }}\n}"
}

sealed class IRStatement {
    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRValStatement -> "val ${binder.prettyPrint()} = ${initializer.prettyPrint()}"
        is IRReturnStatement -> "return ${value.prettyPrint()}"
        is IRCallExpression -> {
            val typeArgs = if (typeArgs == null) {
                ""
            } else {
                "[${typeArgs.joinToString(", ") { it.prettyPrint() }}]"
            }
            val args = "(${this.args.joinToString(", ") { it.prettyPrint() }})"
            "${callee.prettyPrint()}${typeArgs}${args}"
        }
        is IRBool -> value.toString()
        is IRByteString -> "b\"${value.decodeToString()}\""
        is IRVariable -> name.text
        is IRGetStructField -> "${lhs.prettyPrint()}.${rhs.text}"
        IRReturnVoidStatement -> "return void"
    }
}

data class IRValStatement(val binder: IRBinder, val initializer: IRExpression) : IRStatement()
data class IRReturnStatement(val value: IRExpression) : IRStatement()
object IRReturnVoidStatement : IRStatement()

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
) : IRExpression() {

    val name: Name
        get() = when (binding) {
            is IRBinding.FunctionDef -> binding.def.binder.name
            is IRBinding.ExternFunctionDef -> binding.def.externName
            is IRBinding.ValStatement -> binding.statement.binder.name
            is IRBinding.StructDef -> binding.def.globalName
            is IRBinding.ParamRef -> binding.def.params[binding.index].binder.name
        }
}

data class IRGetStructField(
    override val type: Type,
    override val location: SourceLocation,
    val lhs: IRExpression,
    val rhs: Name,
    val index: Int
) : IRExpression()


