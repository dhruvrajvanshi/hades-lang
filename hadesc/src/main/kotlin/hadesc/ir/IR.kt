package hadesc.ir

import hadesc.Name
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
}

class IRModule {
    private val definitions = mutableListOf<IRDefinition>()
    private val globals = mutableMapOf<QualifiedName, IRBinding>()
    fun prettyPrint(): String = definitions.joinToString("\n") { it.prettyPrint() }

    fun addExternFunctionDef(
        name: IRGlobalName,
        type: Type.Function,
        externName: Name,
        paramTypes: List<Type>
    ): IRExternFunctionDef {
        val value = IRExternFunctionDef(this, name, type, externName = externName, paramTypes = paramTypes)
        add(value)
        return value
    }

    fun addGlobalFunctionDef(
        name: IRGlobalName,
        type: Type.Function,
        typeParams: List<IRTypeParam>?,
        params: List<IRParam>,
        body: IRBlock
    ): IRFunctionDef {
        val value = IRFunctionDef(this, name, type, typeParams, params, body)
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
        val exhaustive = when (def) {
            is IRFunctionDef -> globals[def.name.name] = IRBinding.FunctionDef(def)
            is IRStructDef -> globals[def.globalName.name] = IRBinding.StructDef(def)
            is IRExternFunctionDef -> globals[def.name.name] = IRBinding.ExternFunctionDef(def)
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
    fun prettyPrint(): String = when (this) {
        is IRFunctionDef -> "def ${name.prettyPrint()}: ${type.prettyPrint()} = (${params.joinToString(",") { it.prettyPrint() }}) ${body.prettyPrint()}"
        is IRStructDef -> "struct ${this.globalName.prettyPrint()} {" +
                "\n${fields.entries.joinToString("\n") { "  val ${it.key.text}: ${it.value.prettyPrint()};" }}\n}"
        is IRExternFunctionDef -> "extern def ${name.prettyPrint()} = ${externName.text}"
    }
}

class IRFunctionDef(
    override val module: IRModule,
    val name: IRGlobalName,
    val type: Type.Function,
    val typeParams: List<IRTypeParam>?,
    val params: List<IRParam>,
    var body: IRBlock
) : IRDefinition()

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
    val paramTypes: List<Type>,
    val externName: Name
) : IRDefinition()

class IRTypeParam(val name: IRLocalName, val binderLocation: SourceLocation)

class IRBlock(val name: IRLocalName = IRLocalName(Name("entry"))) {
    var statements = mutableListOf<IRStatement>()
    fun prettyPrint(): String = "{\n${statementSequence().joinToString("\n") { "  " + it.prettyPrint() }}\n}"

    operator fun iterator(): Iterator<IRStatement> = statementSequence().iterator()

    private fun statementSequence() = statements.toList().asSequence()
}


class IRBuilder {
    var position: IRBlock? = null

    fun buildRetVoid(): IRReturnVoidStatement {
        return addStatement(IRReturnVoidStatement())
    }

    fun buildConstBool(ty: Type, location: SourceLocation, value: Boolean): IRValue {
        return IRBool(ty, location, value)
    }


    fun <S : IRStatement> addStatement(statement: S): S {
        requireNotNull(position).statements.add(statement)
        return statement
    }

    fun buildByteString(ty: Type, location: SourceLocation, bytes: ByteArray): IRValue {
        return IRByteString(ty, location, bytes)
    }

    fun buildGetStructField(
        ty: Type,
        location: SourceLocation,
        lhs: IRValue,
        field: Name,
        index: Int
    ): IRValue {
        return IRGetStructField(ty, location, lhs = lhs, rhs = field, index = index)
    }

    fun buildVariable(ty: Type, location: SourceLocation, name: IRName): IRValue {
        return IRVariable(ty, location, name)
    }

    fun buildCall(
        type: Type,
        location: SourceLocation,
        callee: IRValue,
        typeArgs: List<Type>?,
        args: List<IRValue>,
        name: IRLocalName
    ): IRValue {
        val call = IRCall(
            type = type,
            location = location,
            callee = callee,
            typeArgs = typeArgs,
            args = args,
            name = name
        )
        val ref = IRVariable(type, location, name)
        addStatement(call)
        return ref
    }

    fun buildReturn(value: IRValue): IRStatement {
        return addStatement(IRReturnStatement(value))
    }

    fun buildAlloca(type: Type, name: IRLocalName): IRStatement {
        return addStatement(IRAlloca(type, name))
    }

    fun buildLoad(name: IRLocalName, type: Type, ptr: IRValue): IRStatement {
        return addStatement(IRLoad(name, type, ptr))
    }

    fun buildStore(ptr: IRValue, value: IRValue): IRStatement {
        return addStatement(IRStore(ptr, value))
    }

    private fun positionAtEnd(block: IRBlock) {
        this.position = block
    }


    fun withinBlock(block: IRBlock, function: () -> Unit) {
        val oldPosition = position
        positionAtEnd(block)
        function()
        position = oldPosition
    }
}

sealed class IRStatement {
    override fun toString(): String = prettyPrint()

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRReturnStatement -> "return ${value.prettyPrint()}"
        is IRReturnVoidStatement -> "return void"
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
    }
}

class IRReturnStatement(
    val value: IRValue
) : IRStatement()

class IRAlloca(
    val type: Type,
    val name: IRLocalName
) : IRStatement()

class IRStore(
    val ptr: IRValue,
    val value: IRValue
) : IRStatement()

class IRLoad(
    val name: IRLocalName,
    val type: Type,
    val ptr: IRValue
) : IRStatement()

class IRReturnVoidStatement : IRStatement()


sealed class IRValue {

    abstract val type: Type
    abstract val location: SourceLocation

    override fun toString(): String {
        return prettyPrint()
    }

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRBool -> value.toString()
        is IRByteString -> "b\"${value.decodeToString()}\""
        is IRVariable -> "${type.prettyPrint()} ${name.prettyPrint()}"
        is IRGetStructField -> "${lhs.prettyPrint()}.${rhs.text}"
        is IRCIntConstant -> value.toString()
        is IRNullPtr -> "nullptr"
    }
}

data class IRCall(
    val type: Type,
    val location: SourceLocation,
    val callee: IRValue,
    val typeArgs: List<Type>?,
    val args: List<IRValue>,
    val name: IRLocalName
) : IRStatement()

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

