package hadesboot.middle

interface TypeBuilder {
    val usize get() = Type.usize
    val isize get() = Type.isize
    val u32 get() = Type.u32
    val i32 get() = Type.i32
    fun tuple(vararg of: Type): Type.Tuple = Type.Tuple(of.toList())
}

interface ConstantBuilder: TypeBuilder {
    fun u32(value: UInt): Constant.Int = Constant.Int(Type.u32, value.toULong())
    fun i32(value: Int): Constant.Int = Constant.Int(Type.i32, value.toULong())
    fun usize(value: ULong): Constant.Int = Constant.Int(Type.usize, value)
    fun isize(value: Long): Constant.Int = Constant.Int(Type.isize, value.toULong())
}

fun buildBlock(label: String, build: BlockBuilder.() -> Unit): Block {
    val builder = BlockBuilderImpl(label)
    builder.build()
    return builder.run()
}

private class BlockBuilderImpl(private val label: String): BlockBuilder {
    private var terminator: Terminator? = null
    private var instructions = mutableListOf<Instruction>()

    override fun emit(instruction: Instruction) {
        instructions.add(instruction)
    }
    override fun emit(terminator: Terminator) {
        terminate(terminator)
    }

    override fun emitReturn(value: Value) {
        terminate(Terminator.Return(value))
    }

    private fun terminate(terminator: Terminator) {
        require(this.terminator == null) {
            "Tried to terminate an already terminated block: $label"
        }
        this.terminator = terminator
    }

    fun run(): Block {
        return Block(
            label = label,
            instructions = instructions,
            terminator = requireNotNull(terminator)
        )
    }
}


fun buildFn(name: String, build: FnBuilder.() -> Unit): Fn {
    val builder = FnBuilderImpl(name)
    builder.build()
    return builder.run()
}

interface FnBuilder {
    fun addBlock(block: Block)
    var returnType: Type
    var entry: Block?
}
interface BlockBuilder {
    fun emit(instruction: Instruction)
    fun emit(terminator: Terminator)
    fun emitReturn(value: Value)
}

private class FnBuilderImpl(private val name: String): FnBuilder {
    override var returnType: Type = Type.unit
    override var entry: Block? = null
    private val blocks = mutableListOf<Block>()
    private val parameters = mutableListOf<Parameter>()
    fun run(): Fn {
        return Fn(
            name = name,
            returnType = returnType,
            parameters = parameters,
            entry = requireNotNull(entry) {
                "Entry block not set"
            },
            blocks = blocks,
        )
    }

    override fun addBlock(block: Block) {
        blocks.add(block)
    }
}

fun buildModule(name: String, build: ModuleBuilder.() -> Unit): Module {
    val builder = ModuleBuilderImpl(name)
    builder.build()
    return builder.run()
}

interface ModuleBuilder: TypeBuilder, ConstantBuilder {
    fun addFn(name: String, ctx: FnBuilder.() -> Unit): Fn
}
class ModuleBuilderImpl(private val name: String): ModuleBuilder {
    private val items = mutableListOf<Item>()

    private fun add(item: Item) {
        items.add(item)
    }

    override fun addFn(name: String, ctx: FnBuilder.() -> Unit): Fn {
        val fn = buildFn(name, ctx)
        add(fn)
        return fn
    }

    fun run(): Module = Module(name, items)
}
