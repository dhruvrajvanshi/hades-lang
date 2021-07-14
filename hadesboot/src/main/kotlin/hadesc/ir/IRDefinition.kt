package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRDefinition {
    abstract val module: IRModule
    abstract val location: SourceLocation
    open fun prettyPrint(): String = when (this) {
        is IRFunctionDef -> this.prettyPrint()
        is IRStructDef -> {
            val typeParamsStr = if (typeParams == null) ""
            else "[${typeParams.joinToString(", ") {it.name.prettyPrint()}}]"
            "struct ${this.globalName.prettyPrint()}$typeParamsStr {" +
                    "\n${fields.entries.joinToString("\n") { "  val ${it.key.text}: ${it.value.prettyPrint()};" }}\n}"
        }
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
        is IRExternConstDef -> {
            "extern const ${name.prettyPrint()} : ${type.prettyPrint()} = $externName"
        }
    }
}

data class IRFunctionSignature(
    val location: SourceLocation,
    val name: IRGlobalName,
    val type: Type.Function,
    val typeParams: List<IRTypeParam>?,
    val params: List<IRParam>,
    val constraints: List<IRConstraint>
) {
    fun prettyPrint(): String {
        val typeParamsStr = if (typeParams != null)
            "[" + typeParams.joinToString(", ") { it.name.prettyPrint() } + "]"
        else ""
        val paramsStr = "(" + params.joinToString(", ") { it.prettyPrint() } + ")"
        val constraintsStr = if (constraints.isEmpty()) "" else
            constraints.joinToString(", ") { it.prettyPrint() }
        return "def ${name.prettyPrint()}: ${type.prettyPrint()} = $typeParamsStr$paramsStr$constraintsStr"
    }
}

data class IRConstraint(
        val name: IRLocalName,
        val typeParam: IRTypeParam,
        val interfaceRef: IRInterfaceRef,
        val location: SourceLocation,
        val type: Type
) {
    fun prettyPrint(): String {
        return "${name.prettyPrint()}: ${interfaceRef.prettyPrint()} for ${typeParam.name.name.text} "
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
    override val location: SourceLocation,
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

    fun getBlock(name: IRLocalName): IRBlock? {
        return blocks.find { it.name == name }
    }

    override fun prettyPrint(): String {
        return "// $location\ndef ${name.prettyPrint()}(${params.joinToString(",") { it.prettyPrint() }}): ${type.to.prettyPrint()} {" +
                "${entryBlock.prettyPrint()}\n${blocks.joinToString(""){ it.prettyPrint() }}}"
    }
}

data class IRInterfaceDef(
        override val module: IRModule,
        override val location: SourceLocation,
        val name: IRGlobalName,
        val typeParams: List<IRTypeParam>?,
        val members: List<IRFunctionSignature>
) : IRDefinition()

data class IRImplementationDef(
        override val module: IRModule,
        override val location: SourceLocation,
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
        override val location: SourceLocation,
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
        override val location: SourceLocation,
        val constructorType: Type,
        val instanceType: Type,
        val globalName: IRGlobalName,
        val typeParams: List<IRTypeParam>?,
        val fields: Map<Name, Type>
) : IRDefinition()

class IRExternFunctionDef(
        override val module: IRModule,
        override val location: SourceLocation,
        val name: IRGlobalName,
        val type: Type.Function,
        val externName: Name
) : IRDefinition()

class IRExternConstDef(
    override val module: IRModule,
    override val location: SourceLocation,
    val name: IRGlobalName,
    val type: Type,
    val externName: Name,
) : IRDefinition()