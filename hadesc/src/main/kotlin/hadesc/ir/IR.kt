package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import java.nio.charset.StandardCharsets

sealed class IRValueName : IRExpression {
    override fun prettyPrint(): String = when (this) {
        is IRGlobalName -> "@${qualifiedName.names.joinToString(".") { it.text }}"
        is IRLocalName -> "%${name.text}"
    }
}

data class IRGlobalName(
    override val type: Type,
    val qualifiedName: QualifiedName
) : IRValueName(), IRExpression

data class IRLocalName(
    override val type: Type,
    val name: Name
) : IRValueName(), IRExpression

data class IRLabelName(
    val name: Name
) {
    fun prettyPrint(): String = ".${name.text}"
}

typealias IRType = Type

class IRBuilder(val module: IRModule) {
    lateinit var basicBlock: IRBasicBlock
    var offsetInBlock = 0
    fun positionAtStart(basicBlock: IRBasicBlock) {
        this.basicBlock = basicBlock
        offsetInBlock = 0
    }

    fun buildCall(
        type: Type,
        callee: IRExpression,
        typeArgs: List<Type>,
        args: List<IRExpression>
    ): IRExpression {
        val name = module.generateUniqueLocal(type)

        addInstruction(IRCall(type, name, callee, typeArgs, args))

        return name
    }

    private fun addInstruction(instruction: IRInstruction) {
        basicBlock.addAtOffset(instruction, offsetInBlock)
    }
}

class IRModule {
    private val globals = mutableMapOf<IRValueName, IRGlobalDeclaration>()
    private var nextNameIndex = 0
    fun addFunction(location: SourceLocation, name: IRGlobalName, type: Type): IRFunctionDeclaration {
        require(globals[name] == null) { "Duplicate add of function at $location" }
        val func = IRFunctionDeclaration(location, name, type)
        globals[name] = func
        return func
    }

    fun generateUniqueLocal(type: Type): IRLocalName {
        nextNameIndex++
        val name = Name("$nextNameIndex")
        return IRLocalName(type, name)
    }
}

interface IRExpression {
    fun prettyPrint(): String

    val type: Type
}

data class IRByteString(
    override val type: Type,
    val location: SourceLocation,
    val bytes: ByteArray
) : IRExpression {
    override fun prettyPrint(): String {
        return "b\"${bytes.toString(StandardCharsets.UTF_8)}\""
    }
}

sealed class IRGlobalDeclaration {
    abstract val location: SourceLocation
    abstract val name: IRValueName
}

data class IRBasicBlock(val name: IRLabelName) {
    internal val instructions: MutableList<IRInstruction> = mutableListOf()

    fun addAtOffset(instruction: IRInstruction, offsetInBlock: Int) {
        require(instructions.size == offsetInBlock) {
            "Can't insert to middle of basic block yet"
        }
        instructions.add(instruction)
    }
}

data class IRFunctionDeclaration internal constructor(
    override val location: SourceLocation,
    override val name: IRGlobalName,
    val type: Type,
    private val basicBlocks: MutableList<IRBasicBlock> = mutableListOf()
) : IRGlobalDeclaration() {
    fun prettyPrint(): String {
        val blocks = basicBlocks.joinToString("\n") {
            "${it.name.prettyPrint()}:\n" + it.instructions
                .joinToString("\n") { "  ${it.prettyPrint()}" }
        }
        return "def ${name.prettyPrint()}: ${type.prettyPrint()} = {\n${blocks}\n}"
    }

    private fun addBasicBlock(name: IRLabelName): IRBasicBlock {
        val bb = IRBasicBlock(name)
        basicBlocks.add(bb)
        return bb
    }

    fun addBasicBlock(str: String): IRBasicBlock {
        return addBasicBlock(IRLabelName(Name(str)))
    }
}

sealed class IRInstruction {
    fun prettyPrint(): String = when (this) {
        is IRCall -> callee.prettyPrint() +
                "[${typeArgs.joinToString(",") { it.prettyPrint() }}]" +
                "(${args.joinToString(",") { it.prettyPrint() }})"
    }
}

data class IRCall(
    override val type: Type,
    val name: IRValueName,
    val callee: IRExpression,
    val typeArgs: List<Type>,
    val args: List<IRExpression>
) : IRInstruction(), IRExpression

