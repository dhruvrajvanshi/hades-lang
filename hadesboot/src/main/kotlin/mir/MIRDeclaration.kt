package mir

import java.nio.file.Path

sealed interface MIRDeclaration {
    sealed interface GlobalDef {
        val name: String
        val type: MIRType
    }
    data class ExternFunction(
        override val name: String,
        val paramTypes: List<MIRType>,
        val returnType: MIRType,
    ): MIRDeclaration, GlobalDef {
        override val type: MIRType.Function
            get() = MIRType.Function(paramTypes, returnType)
    }

    data class Function(
        override val name: String,
        val params: List<MIRParam>,
        val returnType: MIRType,
        val basicBlocks: List<MIRBasicBlock>,
    ) : MIRDeclaration, GlobalDef {
        init {
            require(basicBlocks.isNotEmpty()) {
                "Function must have at least one basic block."
            }
        }

        override val type: MIRType.Function
            get() = MIRType.Function(
                paramTypes = params.map { it.type },
                returnType = returnType,
            )
        val entryBlock get(): MIRBasicBlock = basicBlocks.first()
    }

    data class StaticDefinition(
        override val name: String,
        override val type: MIRType,
        val initializer: MIRValue,
    ) : MIRDeclaration, GlobalDef
}

data class MIRParam(
    val name: String,
    val type: MIRType,
)

class MIRFunctionBuilder(
    val name: String,
    val returnType: MIRType,
    var location: MIRLocation,
    private val moduleBuilder: MIRModuleBuilder,
) {
    internal val params = mutableListOf<MIRParam>()
    private val blocks = mutableListOf<MIRBasicBlock>()
    internal val locals = mutableMapOf<String, MIRType>()
    fun addParam(name: String, type: MIRType) {
        params.add(MIRParam(name, type))
    }

    private fun addBlock(block: MIRBasicBlock) {
        blocks.add(block)
    }

    fun addBlock(name: String, runBlock: (MIRBasicBlockBuilder).() -> Unit) {
        val builder = MIRBasicBlockBuilder(name, location, this, moduleBuilder)
        builder.runBlock()
        addBlock(builder.build())
        location = builder.location
    }

    fun cstr(text: String): MIRValue.CStrLiteral {
        return MIRValue.CStrLiteral(text)
    }

    internal fun build(): MIRDeclaration.Function = MIRDeclaration.Function(name, params, returnType, blocks)
}

fun MIRModuleBuilder.buildFunction(
    name: String,
    path: Path,
    returnType: MIRType,
    runEntryBlock: MIRFunctionBuilder.() -> Unit
): MIRDeclaration.Function {
    val fnBuilder = MIRFunctionBuilder(name, returnType, MIRLocation(1, 1, path), this)
    fnBuilder.runEntryBlock()
    return fnBuilder.build()
}