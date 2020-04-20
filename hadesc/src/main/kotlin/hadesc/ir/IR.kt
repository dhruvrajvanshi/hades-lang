package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRBinding {
    class FunctionDef(val def: IRFunctionDef) : IRBinding()
    class ExternFunctionDef(val def: IRExternFunctionDef) : IRBinding()
    class Local(val name: Name) : IRBinding()
    class StructDef(val def: IRStructDef) : IRBinding()
    class ParamRef(val def: IRFunctionDef, val index: Int) : IRBinding()
}

class IRModule {
    val definitions = mutableListOf<IRDefinition>()
    fun prettyPrint(): String = definitions.joinToString("\n") { it.prettyPrint() }

    fun addExternFunctionDef(binder: IRBinder, externName: Name, paramTypes: List<Type>): IRExternFunctionDef {
        val value = IRExternFunctionDef(this, binder, externName = externName, paramTypes = paramTypes)
        definitions.add(value)
        return value
    }

    fun addGlobalFunctionDef(
        binder: IRBinder,
        typeParams: List<IRTypeBinder>?,
        params: List<IRParam>,
        body: IRBlock
    ): IRFunctionDef {
        val value = IRFunctionDef(this, binder, typeParams, params, body)
        definitions.add(value)
        return value
    }

    fun addStructDef(
        ty: Type,
        instanceType: Type,
        name: Name,
        typeParams: List<IRTypeBinder>?,
        fields: Map<Name, Type>
    ): IRStructDef {
        val value = IRStructDef(this, ty, instanceType, name, typeParams, fields)
        definitions.add(value)
        return value

    }

    operator fun iterator(): Iterator<IRDefinition> = definitions.iterator()

    fun add(def: IRDefinition) {
        definitions.add(def)
    }
}

sealed class IRDefinition {
    abstract val module: IRModule
    fun prettyPrint(): String = when (this) {
        is IRFunctionDef -> "def ${binder.prettyPrint()} = (${params.joinToString(", ") { it.prettyPrint() }}) ${body.prettyPrint()}"
        is IRStructDef -> "struct ${this.globalName.text} {" +
                "\n${fields.entries.joinToString("\n") { "  val ${it.key.text}: ${it.value.prettyPrint()};" }}\n}"
        is IRExternFunctionDef -> "extern def ${binder.prettyPrint()} = ${externName.text}"
    }
}

class IRFunctionDef(
    override val module: IRModule,
    val binder: IRBinder,
    val typeParams: List<IRTypeBinder>?,
    val params: List<IRParam>,
    var body: IRBlock
) : IRDefinition() {
    val type get() = binder.type
}

class IRStructDef(
    override val module: IRModule,
    val constructorType: Type,
    val instanceType: Type,
    val globalName: Name,
    val typeParams: List<IRTypeBinder>?,
    val fields: Map<Name, Type>
) : IRDefinition()

class IRExternFunctionDef(
    override val module: IRModule,
    val binder: IRBinder,
    val paramTypes: List<Type>,
    val externName: Name
) : IRDefinition() {
    val type get() = binder.type
}

data class IRBinder(val name: Name, val type: Type) {
    fun prettyPrint(): String = "${name.text} : ${type.prettyPrint()}"
}

data class IRParam(val binder: IRBinder, val location: SourceLocation) {
    fun prettyPrint(): String = "${binder.name.text} : ${binder.type.prettyPrint()}"
}

class IRTypeBinder(val name: Name, val binderLocation: SourceLocation)

class IRBlock {
    var startNode: IRStatementNode = IRStatementNode(null, IREmptyStatement(null), null)
    fun prettyPrint(): String = "{\n${statementSequence().joinToString("\n") { "  " + it.prettyPrint() }}\n}"

    operator fun iterator(): Iterator<IRStatement> = statementSequence().iterator()

    private fun statementSequence() =
        sequence<IRStatement> {
            var node: IRStatementNode? = startNode
            while (node != null) {
                yield(node.statement)
                node = node.next
            }
        }
}

class IRStatementNode(
    var previous: IRStatementNode?,
    val statement: IRStatement,
    var next: IRStatementNode?
)

class IRBuilder {
    var position: IRStatementNode? = null

    private val previousNode get() = requireNotNull(position)

    fun buildRetVoid(): IRReturnVoidStatement {
        return addStatement(IRReturnVoidStatement(previousNode))
    }

    fun buildConstBool(ty: Type, location: SourceLocation, value: Boolean): IRValue {
        return IRBool(previousNode, ty, location, value)
    }


    fun <S : IRStatement> addStatement(statement: S): S {
        val newNode = IRStatementNode(previous = position, statement = statement, next = position?.next)
        position?.next?.previous = newNode
        position?.next = newNode
        position = newNode
        return statement
    }

    fun buildByteString(ty: Type, location: SourceLocation, bytes: ByteArray): IRValue {
        return IRByteString(previousNode, ty, location, bytes)
    }

    fun buildGetStructField(
        ty: Type,
        location: SourceLocation,
        lhs: IRValue,
        name: Name,
        index: Int
    ): IRValue {
        return IRGetStructField(previousNode, ty, location, lhs = lhs, rhs = name, index = index)
    }

    fun buildVariable(ty: Type, location: SourceLocation, irBinding: IRBinding): IRValue {
        return IRVariable(previousNode, ty, location, irBinding)
    }

    fun buildCall(
        type: Type,
        location: SourceLocation,
        callee: IRValue,
        typeArgs: List<Type>?,
        args: List<IRValue>,
        name: Name
    ): IRValue {
        val call = IRCall(
            previousNode,
            type = type,
            location = location,
            callee = callee,
            typeArgs = typeArgs,
            args = args,
            name = name
        )
        val ref = IRVariable(previousNode, type, location, binding = IRBinding.Local(name))
        addStatement(call)
        return ref
    }

    fun buildReturn(value: IRValue): IRStatement {
        return addStatement(IRReturnStatement(previousNode, value))
    }

    fun buildValStatement(binder: IRBinder, expr: IRValue): IRValStatement {
        return addStatement(IRValStatement(previousNode, binder, expr))
    }

    private fun positionAtEnd(block: IRBlock) {
        var node: IRStatementNode? = block.startNode
        while (node != null) {
            position = node
            node = node.next
        }
    }


    fun withinBlock(block: IRBlock, function: () -> Unit) {
        val oldPosition = position
        positionAtEnd(block)
        function()
        position = oldPosition
    }
}

sealed class IRStatement {
    abstract val previousNode: IRStatementNode?

    override fun toString(): String = prettyPrint()

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRValStatement -> "val ${binder.prettyPrint()} = ${initializer.prettyPrint()}"
        is IRReturnStatement -> "return ${value.prettyPrint()}"
        is IRReturnVoidStatement -> "return void"
        is IREmptyStatement -> ""
        is IRCall -> {
            val typeArgs = if (typeArgs == null) {
                ""
            } else {
                "[${typeArgs.joinToString(", ") { it.prettyPrint() }}]"
            }
            val args = "(${this.args.joinToString(", ") { it.prettyPrint() }})"
            "${name.text} = ${callee.prettyPrint()}${typeArgs}${args}"
        }
    }

    fun removeFromParent() {
        require(previousNode?.next?.statement as Any === this)
        previousNode?.next = previousNode?.next?.next
    }
}

class IRValStatement(
    override val previousNode: IRStatementNode?,
    val binder: IRBinder,
    val initializer: IRValue
) : IRStatement()

class IRReturnStatement(
    override val previousNode: IRStatementNode?,
    val value: IRValue
) : IRStatement()

class IRReturnVoidStatement(
    override val previousNode: IRStatementNode?
) : IRStatement()

class IREmptyStatement(
    override val previousNode: IRStatementNode?
) : IRStatement()

sealed class IRValue {
    abstract val previousNode: IRStatementNode?
    abstract val type: Type
    abstract val location: SourceLocation

    override fun toString(): String {
        return prettyPrint()
    }

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRBool -> value.toString()
        is IRByteString -> "b\"${value.decodeToString()}\""
        is IRVariable -> name.text
        is IRGetStructField -> "${lhs.prettyPrint()}.${rhs.text}"
    }
}

class IRCall(
    override val previousNode: IRStatementNode?,
    val type: Type,
    val location: SourceLocation,
    val callee: IRValue,
    val typeArgs: List<Type>?,
    val args: List<IRValue>,
    val name: Name
) : IRStatement()

class IRBool(
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val value: Boolean
) : IRValue()

class IRByteString(
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val value: ByteArray
) : IRValue()

class IRVariable(
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val binding: IRBinding
) : IRValue() {

    val name: Name
        get() = when (binding) {
            is IRBinding.FunctionDef -> binding.def.binder.name
            is IRBinding.ExternFunctionDef -> binding.def.externName
            is IRBinding.Local -> binding.name
            is IRBinding.StructDef -> binding.def.globalName
            is IRBinding.ParamRef -> binding.def.params[binding.index].binder.name
        }
}

class IRGetStructField(
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val lhs: IRValue,
    val rhs: Name,
    val index: Int
) : IRValue()


