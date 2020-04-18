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
        instanceType: Type.Struct,
        name: Name,
        typeParams: List<IRTypeBinder>?,
        fields: Map<Name, Type>
    ): IRStructDef {
        val value = IRStructDef(this, ty, instanceType, name, typeParams, fields)
        definitions.add(value)
        return value

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

data class IRFunctionDef(
    override val module: IRModule,
    val binder: IRBinder,
    val typeParams: List<IRTypeBinder>?,
    val params: List<IRParam>,
    var body: IRBlock
) : IRDefinition() {
    val type get() = binder.type
}

data class IRStructDef(
    override val module: IRModule,
    val constructorType: Type,
    val instanceType: Type,
    val globalName: Name,
    val typeParams: List<IRTypeBinder>?,
    val fields: Map<Name, Type>
) : IRDefinition()

data class IRExternFunctionDef(
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

inline class IRParam(val binder: IRBinder) {
    fun prettyPrint(): String = "${binder.name.text} : ${binder.type.prettyPrint()}"
}

data class IRTypeBinder(val name: Name)

class IRBlock {
    private var startNode: IRStatementNode? = null
    internal var endNode: IRStatementNode? = null
    fun prettyPrint(): String = "{\n${statementSequence().joinToString("\n") { "  " + it.prettyPrint() }}\n}"

    fun add(statement: IRStatement) {
        if (startNode == null) {
            require(endNode == null)
            val node = IRStatementNode(null, statement, null)
            startNode = node
            endNode = node
        } else {
            val previous = endNode
            require(previous != null)
            val node = IRStatementNode(endNode, statement, null)
            previous.next = node
            node.previous = previous
            endNode = node
        }
    }

    operator fun iterator(): Iterator<IRStatement> = statementSequence().iterator()

    private fun statementSequence() =
        sequence<IRStatement> {
            var node = startNode
            while (node != null) {
                yield(node.statement)
                node = node.next
            }
        }
}

data class IRStatementNode(
    var previous: IRStatementNode?,
    val statement: IRStatement,
    var next: IRStatementNode?
)

class IRBuilder {
    private var _block: IRBlock? = null
    val block: IRBlock
        get() = requireNotNull(_block)

    val blockOrNull get() = _block

    private val previousNode get() = block.endNode

    fun buildRetVoid(): IRReturnVoidStatement {
        return addStatement(IRReturnVoidStatement(previousNode, block))
    }

    fun buildConstBool(ty: Type, location: SourceLocation, value: Boolean): IRValue {
        return IRBool(block, previousNode, ty, location, value)
    }


    private fun <S : IRStatement> addStatement(statement: S): S {
        block.add(statement)
        return statement
    }

    fun buildByteString(ty: Type, location: SourceLocation, bytes: ByteArray): IRValue {
        return IRByteString(block, previousNode, ty, location, bytes)
    }

    fun buildGetStructField(
        ty: Type,
        location: SourceLocation,
        lhs: IRValue,
        name: Name,
        index: Int
    ): IRValue {
        return IRGetStructField(block, previousNode, ty, location, lhs = lhs, rhs = name, index = index)
    }

    fun buildVariable(ty: Type, location: SourceLocation, irBinding: IRBinding): IRValue {
        return IRVariable(block, previousNode, ty, location, irBinding)
    }

    fun buildCall(
        type: Type,
        location: SourceLocation,
        callee: IRValue,
        typeArgs: List<Type>?,
        args: List<IRValue>
    ): IRValue {
        return IRCall(
            block,
            previousNode,
            type = type,
            location = location,
            callee = callee,
            typeArgs = typeArgs,
            args = args
        )
    }

    fun buildReturn(value: IRValue): IRStatement {
        return addStatement(IRReturnStatement(block, previousNode, value))
    }

    fun buildValStatement(binder: IRBinder, expr: IRValue): IRValStatement {
        return addStatement(IRValStatement(block, previousNode, binder, expr))
    }

    fun buildExprStatement(expr: IRValue): IRStatement {
        return addStatement(IRExpressionStatement(block, previousNode, expr))
    }

    fun positionAtBlock(block: IRBlock) {
        _block = block
    }
}

sealed class IRStatement {
    abstract val block: IRBlock
    abstract val previousNode: IRStatementNode?

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRValStatement -> "val ${binder.prettyPrint()} = ${initializer.prettyPrint()}"
        is IRReturnStatement -> "return ${value.prettyPrint()}"
        is IRReturnVoidStatement -> "return void"
        is IRExpressionStatement -> expression.prettyPrint()
    }
}

data class IRValStatement(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    val binder: IRBinder,
    val initializer: IRValue
) : IRStatement()

data class IRExpressionStatement(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    val expression: IRValue
) : IRStatement()

data class IRReturnStatement(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    val value: IRValue
) : IRStatement()

data class IRReturnVoidStatement(
    override val previousNode: IRStatementNode?,
    override val block: IRBlock
) : IRStatement()

sealed class IRValue {
    abstract val block: IRBlock
    abstract val previousNode: IRStatementNode?
    abstract val type: Type
    abstract val location: SourceLocation

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRCall -> {
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
    }
}

data class IRCall(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val callee: IRValue,
    val typeArgs: List<Type>?,
    val args: List<IRValue>
) : IRValue()

data class IRBool(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val value: Boolean
) : IRValue()

data class IRByteString(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val value: ByteArray
) : IRValue()

data class IRVariable(
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val binding: IRBinding
) : IRValue() {

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
    override val block: IRBlock,
    override val previousNode: IRStatementNode?,
    override val type: Type,
    override val location: SourceLocation,
    val lhs: IRValue,
    val rhs: Name,
    val index: Int
) : IRValue()


