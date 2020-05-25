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
        is IRInterfaceDef -> {
            val params = if (typeParams != null) {
                "[" + typeParams.joinToString(",") { it.name.prettyPrint() } + "]"
            } else ""
            "interface ${name.prettyPrint()}$params {\n" +
                    members.joinToString("\n") { "  " + it.prettyPrint() }
                    "\n}"
        }
        is IRImplementationDef -> {
            val body = "{\n" +
                    implementations.joinToString("\n") { "  " + it.prettyPrint() } +
            "\n}"
            "implementation ${name.prettyPrint()} : ${interfaceRef.prettyPrint()} for ${forType.prettyPrint()} $body"
        }
    }
}

data class IRFunctionSignature(
    val location: SourceLocation,
    val name: IRGlobalName,
    val type: Type.Function,
    val typeParams: List<IRTypeParam>?,
    val receiverType: Type?,
    val params: List<IRParam>,
    val constraints: List<IRConstraint>
) {
    init {
        if (receiverType == null) {
            require(type.receiver == null)
        } else {
            requireNotNull(type.receiver)
        }
    }
    fun prettyPrint(): String {
        val typeParamsStr = if (typeParams != null)
            "[" + typeParams.joinToString(", ") { it.name.prettyPrint() } + "]"
        else ""
        val receiverStr = if (receiverType != null) {
            "this: ${receiverType.prettyPrint()}, "
        } else ""
        val paramsStr = "(" + receiverStr + params.joinToString(", ") { it.prettyPrint() } + ")"
        val constraintsStr = if (constraints.isEmpty()) "" else
            constraints.joinToString(", ") { it.prettyPrint() }
        return "def ${name.prettyPrint()}: ${type.prettyPrint()} = $typeParamsStr$paramsStr$constraintsStr"
    }
}

data class IRConstraint(
        val forType: Type,
        val interfaceRef: IRInterfaceRef
) {
    fun prettyPrint(): String {
        return "${interfaceRef.prettyPrint()} for $forType "
    }
}

data class IRInterfaceRef(
    val name: IRGlobalName,
    val typeArgs: List<Type>
) {
    fun prettyPrint(): String {
        val typeArgsStr = if (typeArgs.isEmpty()) {
            ""
        } else {
            typeArgs.joinToString(", ") { it.prettyPrint() }
        }
        return "${name.prettyPrint()}$typeArgsStr"
    }
}

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

data class IRInterfaceDef(
        override val module: IRModule,
        val name: IRGlobalName,
        val typeParams: List<IRTypeParam>?,
        val members: List<IRFunctionSignature>
) : IRDefinition()

data class IRImplementationDef(
        override val module: IRModule,
        val name: IRGlobalName,
        val interfaceRef: IRInterfaceRef,
        val forType: Type,
        val implementations: List<IRFunctionDef>
): IRDefinition()

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