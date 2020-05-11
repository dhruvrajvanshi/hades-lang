package hadesc.ir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class IRName {
    fun prettyPrint() = when (this) {
        is IRLocalName -> "%${name.text}"
        is IRGlobalName -> "@${name.mangle()}"
    }

    fun mangle() = when (this) {
        is IRLocalName -> name.text
        is IRGlobalName -> name.mangle()
    }

}

data class IRLocalName(val name: Name) : IRName()
data class IRGlobalName(val name: QualifiedName) : IRName()


sealed class IRBinding {
    abstract val type: Type

    class FunctionDef(val def: IRFunctionDef) : IRBinding() {
        override val type: Type
            get() = def.type

    }

    class ExternFunctionDef(val def: IRExternFunctionDef) : IRBinding() {
        override val type: Type
            get() = def.type
    }

    class StructDef(val def: IRStructDef) : IRBinding() {
        override val type: Type
            get() = def.constructorType

    }

    class ConstDef(val def: IRConstDef) : IRBinding() {
        override val type: Type get() = def.type
    }
}

class IRModule {
    private val definitions = mutableListOf<IRDefinition>()
    private val globals = mutableMapOf<QualifiedName, IRBinding>()
    fun prettyPrint(): String = definitions.joinToString("\n") { it.prettyPrint() }

    fun addExternFunctionDef(
        name: IRGlobalName,
        type: Type.Function,
        externName: Name
    ): IRExternFunctionDef {
        val value = IRExternFunctionDef(this, name, type, externName = externName)
        add(value)
        return value
    }

    fun addConstDef(
        name: IRGlobalName,
        type: Type,
        initializer: IRValue
    ): IRConstDef {
        val def = IRConstDef(this, name, type, initializer)
        add(def)
        return def
    }

    fun addGlobalFunctionDef(
        name: IRGlobalName,
        type: Type.Function,
        typeParams: List<IRTypeParam>?,
        params: List<IRParam>,
        entryBlock: IRBlock
    ): IRFunctionDef {
        val value = IRFunctionDef(this, name, type, typeParams, params, entryBlock, blocks = mutableListOf())
        add(value)
        return value
    }

    fun addStructDef(
        constructorType: Type.Function,
        instanceType: Type,
        name: IRGlobalName,
        typeParams: List<IRTypeParam>?,
        fields: Map<Name, Type>
    ): IRStructDef {
        val value = IRStructDef(this, constructorType, instanceType, name, typeParams, fields)
        add(value)
        return value

    }

    operator fun iterator(): Iterator<IRDefinition> = definitions.iterator()

    fun add(def: IRDefinition) {
        definitions.add(def)
        return when (def) {
            is IRFunctionDef -> globals[def.name.name] = IRBinding.FunctionDef(def)
            is IRStructDef -> globals[def.globalName.name] = IRBinding.StructDef(def)
            is IRExternFunctionDef -> globals[def.name.name] = IRBinding.ExternFunctionDef(def)
            is IRConstDef -> globals[def.name.name] = IRBinding.ConstDef(def)
        }
    }

    fun resolveGlobal(name: IRGlobalName): IRBinding {
        return resolveGlobal(name.name)
    }

    fun resolveGlobal(name: QualifiedName): IRBinding {
        return requireNotNull(globals[name]) {
            "Global ${name.mangle()} not present in module"
        }
    }
}

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

class IRFunctionDef(
    override val module: IRModule,
    val name: IRGlobalName,
    val type: Type.Function,
    val typeParams: List<IRTypeParam>?,
    val params: List<IRParam>,
    var entryBlock: IRBlock,
    var blocks: MutableList<IRBlock>
) : IRDefinition() {
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

class IRTypeParam(val name: IRLocalName, val binderLocation: SourceLocation)

class IRBlock(val name: IRLocalName = IRLocalName(Name("entry"))) {
    var statements = mutableListOf<IRInstruction>()
    fun prettyPrint(): String =
        "\n${name.prettyPrint()}:\n${statementSequence().joinToString("\n") { "  " + it.prettyPrint() }}\n"

    operator fun iterator(): Iterator<IRInstruction> = statementSequence().iterator()

    private fun statementSequence() = statements.toList().asSequence()
}

sealed class IRInstruction {
    override fun toString(): String = prettyPrint()

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRReturnInstruction -> "return ${value.prettyPrint()}"
        is IRReturnVoidInstruction -> "return void"
        is IRCall -> {
            val typeArgs = if (typeArgs == null) {
                ""
            } else {
                "[${typeArgs.joinToString(", ") { it.prettyPrint() }}]"
            }
            val args = "(${this.args.joinToString(", ") { it.prettyPrint() }})"
            "${name.prettyPrint()}: ${type.prettyPrint()} = call ${type.prettyPrint()} ${callee.prettyPrint()}${typeArgs}${args}"
        }
        is IRAlloca -> "${name.prettyPrint()}: ${Type.RawPtr(type).prettyPrint()} = alloca ${type.prettyPrint()}"
        is IRStore -> "store ${ptr.prettyPrint()} ${value.prettyPrint()}"
        is IRLoad -> "${name.prettyPrint()}: ${type.prettyPrint()} = load ${ptr.prettyPrint()}"
        is IRNot -> "${name.prettyPrint()}: ${type.prettyPrint()} = not ${arg.prettyPrint()}"
        is IRBr -> "br ${condition.prettyPrint()} then:${ifTrue.prettyPrint()} else:${ifFalse.prettyPrint()}"
        is IRJump -> "jmp ${label.prettyPrint()}"
        is IRBinOp -> "${name.prettyPrint()}: ${type.prettyPrint()} = ${operator.prettyPrint()} ${lhs.prettyPrint()} ${rhs.prettyPrint()}"
    }
}

class IRReturnInstruction(
    val value: IRValue
) : IRInstruction()

class IRAlloca(
    val type: Type,
    val name: IRLocalName
) : IRInstruction()

class IRStore(
    val ptr: IRValue,
    val value: IRValue
) : IRInstruction()

class IRLoad(
    val name: IRLocalName,
    val type: Type,
    val ptr: IRValue
) : IRInstruction()

object IRReturnVoidInstruction : IRInstruction()

class IRBinOp(
    val type: Type,
    val name: IRLocalName,
    val lhs: IRValue,
    val operator: BinaryOperator,
    val rhs: IRValue
) : IRInstruction()


sealed class IRValue : HasLocation {

    abstract val type: Type

    override fun toString(): String {
        return prettyPrint()
    }

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRBool -> value.toString()
        is IRByteString -> "b\"${value.decodeToString()}\""
        is IRVariable -> name.prettyPrint()
        is IRGetStructField -> "${lhs.prettyPrint()}.${rhs.text}"
        is IRCIntConstant -> value.toString()
        is IRNullPtr -> "nullptr"
        is IRMethodRef -> "${thisArg.prettyPrint()}.${method.prettyPrint()}"
    }
}

data class IRCall(
    val type: Type,
    val location: SourceLocation,
    val callee: IRValue,
    val typeArgs: List<Type>?,
    val args: List<IRValue>,
    val name: IRLocalName
) : IRInstruction()

data class IRNot(
    val type: Type,
    val location: SourceLocation,
    val name: IRLocalName,
    val arg: IRValue
) : IRInstruction()

data class IRBr(
    val location: SourceLocation,
    val condition: IRValue,
    val ifTrue: IRLocalName,
    val ifFalse: IRLocalName
) : IRInstruction()

data class IRJump(
    val location: SourceLocation,
    val label: IRLocalName
) : IRInstruction()

class IRBool(
    override val type: Type,
    override val location: SourceLocation,
    val value: Boolean
) : IRValue()

class IRByteString(
    override val type: Type,
    override val location: SourceLocation,
    val value: ByteArray
) : IRValue()

data class IRVariable(
    override val type: Type,
    override val location: SourceLocation,
    val name: IRName
) : IRValue()

class IRGetStructField(
    override val type: Type,
    override val location: SourceLocation,
    val lhs: IRValue,
    val rhs: Name,
    val index: Int
) : IRValue()

class IRCIntConstant(
    override val type: Type,
    override val location: SourceLocation,
    val value: Int
) : IRValue()

class IRNullPtr(
    override val type: Type,
    override val location: SourceLocation
) : IRValue()

class IRMethodRef(
    override val type: Type,
    override val location: SourceLocation,
    val thisArg: IRValue,
    val method: IRGlobalName
) : IRValue()

