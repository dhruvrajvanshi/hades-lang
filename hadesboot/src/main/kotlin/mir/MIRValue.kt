package mir

sealed interface MIRValue {
    data class Object(val values: Map<String, MIRValue>): MIRValue
    data class I32(val value: Int): MIRValue
    data class Function(
        val params: List<MIRParam>,
        val returnType: MIRType,
        val basicBlocks: List<MIRBasicBlock>,
    ): MIRValue {
        init {
            require(basicBlocks.isNotEmpty()) {
                "Function must have at least one basic block."
            }
        }
        val entryBlock get(): MIRBasicBlock = basicBlocks.first()
    }
}

data class MIRParam(
    val name: String,
    val type: MIRType,
)

class MIRValueObjectBuilder {
    private val values = mutableMapOf<String, MIRValue>()

    fun addValue(name: String, value: MIRValue) {
        values[name] = value
    }

    internal fun build(): MIRValue.Object = MIRValue.Object(values)
}

fun buildObject(run: MIRValueObjectBuilder.() -> Unit): MIRValue.Object {
    val builder = MIRValueObjectBuilder()

    builder.run()
    return builder.build()
}

class MIRFunctionBuilder(
    val returnType: MIRType,
) {
    private val params = mutableListOf<MIRParam>()
    private val blocks = mutableListOf<MIRBasicBlock>()
    fun addParam(name: String, type: MIRType) {
        params.add(MIRParam(name, type))
    }

    private fun addBlock(block: MIRBasicBlock) {
        blocks.add(block)
    }

    fun addBlock(name: String, runBlock: (MIRBasicBlockBuilder).() -> Unit) {
        val builder = MIRBasicBlockBuilder(name)
        builder.runBlock()
        addBlock(builder.build())
    }

    internal fun build(): MIRValue.Function = MIRValue.Function(params, returnType, blocks)
}

fun buildFunction(
    returnType: MIRType,
    runEntryBlock: MIRFunctionBuilder.() -> Unit
): MIRValue.Function {
    val fnBuilder = MIRFunctionBuilder(returnType)
    fnBuilder.runEntryBlock()
    return fnBuilder.build()
}