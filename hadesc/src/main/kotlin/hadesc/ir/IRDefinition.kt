package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRDefinition {
    abstract val module: IRModule
    open fun prettyPrint(): String = when (this) {
        is IRFunctionDef -> this.prettyPrint()
        is IRStructDef -> "struct ${this.globalName.prettyPrint()} {" +
                "\n${fields.entries.joinToString("\n") { "  val ${it.key.text}: ${it.value.prettyPrint()};" }}\n}"
        is IRExternFunctionDef -> "extern def ${name.prettyPrint()} = ${externName.text}"
        is IRConstDef -> this.prettyPrint()
    }
}

data class IRFunctionSignature(
    val name: IRGlobalName,
    val type: Type.Function,
    val typeParams: List<IRTypeParam>?,
    val params: List<IRParam>
)

class IRFunctionDef(
    override val module: IRModule,
    val signature: IRFunctionSignature,
    var entryBlock: IRBlock,
    var blocks: MutableList<IRBlock>
) : IRDefinition() {
    val name get() = signature.name
    val type get() = signature.type
    val params get() = signature.params
    val typeParams get() = signature.typeParams
    fun appendBlock(block: IRBlock) {
        blocks.add(block)
    }

    override fun prettyPrint(): String {
        return "def ${name.prettyPrint()}: ${type.prettyPrint()} = (${params.joinToString(",") { it.prettyPrint() }}) {" +
                "${entryBlock.prettyPrint()}\n${blocks.joinToString(""){ it.prettyPrint() }}}"
    }
}

data class IRParam(
        val name: IRLocalName,
        val type: Type,
        val location: SourceLocation,
        val functionName: IRGlobalName,
        val index: Int
) {
    fun prettyPrint(): String {
        return "${name.prettyPrint()}: ${type.prettyPrint()}"
    }
}

class IRConstDef(
        override val module: IRModule,
        val name: IRGlobalName,
        val type: Type,
        val initializer: IRValue
) : IRDefinition() {
    override fun prettyPrint(): String {
        return "const ${name.prettyPrint()}: ${type.prettyPrint()} = ${initializer.prettyPrint()}"
    }
}

class IRStructDef(
        override val module: IRModule,
        val constructorType: Type,
        val instanceType: Type,
        val globalName: IRGlobalName,
        val typeParams: List<IRTypeParam>?,
        val fields: Map<Name, Type>
) : IRDefinition()

class IRExternFunctionDef(
        override val module: IRModule,
        val name: IRGlobalName,
        val type: Type.Function,
        val externName: Name
) : IRDefinition()