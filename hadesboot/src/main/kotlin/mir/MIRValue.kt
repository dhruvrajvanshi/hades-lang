package mir

import java.nio.file.Path

sealed interface MIRValue {
    val type: MIRType
    data class Object(
        override val type: MIRType,
        val values: Map<String, MIRValue>
    ): MIRValue
    data class I32(
        val value: Int
    ): MIRValue {
        override val type get(): MIRType.I32 = MIRType.I32
    }
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

        override val type: MIRType.Function
            get() = MIRType.Function(
                paramTypes = params.map { it.type },
                returnType = returnType,
            )
        val entryBlock get(): MIRBasicBlock = basicBlocks.first()
    }
}

data class MIRParam(
    val name: String,
    val type: MIRType,
)

class MIRValueObjectBuilder {
    private val values = mutableMapOf<String, MIRValue>()
    private val types = mutableMapOf<String, MIRType>()

    fun addValue(name: String, value: MIRValue) {
        values[name] = value
    }

    fun addType(name: String, type: MIRType) {
        types[name] = type
    }

    internal fun build(): MIRValue.Object = MIRValue.Object(
        MIRType.Interface(
            types = types.keys,
            values = values.mapValues { it.value.type }
        ),
        values
    )
}

fun buildObject(run: MIRValueObjectBuilder.() -> Unit): MIRValue.Object {
    val builder = MIRValueObjectBuilder()

    builder.run()
    return builder.build()
}

class MIRFunctionBuilder(
    val returnType: MIRType,
    var location: MIRLocation,
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
        val builder = MIRBasicBlockBuilder(name, location)
        builder.runBlock()
        addBlock(builder.build())
        location = builder.location
    }

    internal fun build(): MIRValue.Function = MIRValue.Function(params, returnType, blocks)
}

fun buildFunction(
    path: Path,
    returnType: MIRType,
    runEntryBlock: MIRFunctionBuilder.() -> Unit
): MIRValue.Function {
    val fnBuilder = MIRFunctionBuilder(returnType, MIRLocation(1, 1, path))
    fnBuilder.runEntryBlock()
    return fnBuilder.build()
}