package mir

import java.nio.file.Path

sealed interface MIRDeclaration {
    data class Function(
        val name: String,
        val params: List<MIRParam>,
        val returnType: MIRType,
        val basicBlocks: List<MIRBasicBlock>,
    ) : MIRDeclaration {
        init {
            require(basicBlocks.isNotEmpty()) {
                "Function must have at least one basic block."
            }
        }

        val type: MIRType.Function
            get() = MIRType.Function(
                paramTypes = params.map { it.type },
                returnType = returnType,
            )
        val entryBlock get(): MIRBasicBlock = basicBlocks.first()
    }

    data class StaticDefinition(
        val name: String,
        val type: MIRType,
        val initializer: MIRValue,
    ) : MIRDeclaration
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
    private val params = mutableListOf<MIRParam>()
    private val blocks = mutableListOf<MIRBasicBlock>()
    private val locals = mutableMapOf<String, MIRType>()
    fun addParam(name: String, type: MIRType) {
        params.add(MIRParam(name, type))
    }

    private fun addBlock(block: MIRBasicBlock) {
        blocks.add(block)
    }

    fun addBlock(name: String, runBlock: (MIRBasicBlockBuilder).() -> Unit) {
        val builder = MIRBasicBlockBuilder(name, location, locals, moduleBuilder)
        builder.runBlock()
        addBlock(builder.build())
        location = builder.location
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