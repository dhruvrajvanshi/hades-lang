package hadesboot.middle

interface TypeBuilderCtx {
    val usize get() = Type.usize
    val isize get() = Type.isize
    val u32 get() = Type.u32
    val i32 get() = Type.i32
    fun tuple(vararg of: Type): Type.Tuple = Type.Tuple(of.toList())
}

interface ConstantBuilderCtx: TypeBuilderCtx {
    fun u32(value: UInt): Constant.Int = Constant.Int(Type.u32, value.toULong())
    fun i32(value: Int): Constant.Int = Constant.Int(Type.i32, value.toULong())
    fun usize(value: ULong): Constant.Int = Constant.Int(Type.usize, value)
    fun isize(value: Long): Constant.Int = Constant.Int(Type.isize, value.toULong())
}

interface ModuleBuilderCtx: TypeBuilderCtx, ConstantBuilderCtx {
    fun addFn(name: String, build: FnBuilderCtx.() -> Unit): Fn
}

interface BlockBuilderCtx: FnBuilderCtx {
    fun emitReturn(value: Value)
}

interface FnBuilderCtx: ModuleBuilderCtx {
    fun param(name: String, type: Type): Parameter
    fun returns(type: Type)
    fun addEntry(build: BlockBuilderCtx.() -> Unit)
    fun addBlock(label: String, build: BlockBuilderCtx.() -> Unit): Block
}
fun buildModule(name: String, build: ModuleBuilderCtx.() -> Unit): Module {
    val items = mutableListOf<Item>()
    val module = Module(name = name, items = items)
    val ctx = object : ModuleBuilderCtx {
        override fun addFn(name: String, build: FnBuilderCtx.() -> Unit): Fn {
            val params = mutableListOf<Parameter>()
            var returnType: Type? = null
            var entry: Block? = null
            val blocks = mutableListOf<Block>()
            val fnBuilder = object : FnBuilderCtx, ModuleBuilderCtx by this {
                override fun param(name: String, type: Type): Parameter {
                    val p = Parameter(name, type)
                    params.add(p)
                    return p
                }

                override fun returns(type: Type) {
                    require(returnType == null) {
                        "return type must be set only once; Previous value: $returnType; New value: $type"
                    }
                    returnType = type
                }

                override fun addEntry(build: BlockBuilderCtx.() -> Unit) {
                    require(entry == null) {
                        "entry must be set only once; Previous value: $entry; New value: Block"
                    }
                    val block = makeBlockBuilder("entry", this)
                    block.build()
                    entry = block.run()
                }

                override fun addBlock(label: String, build: BlockBuilderCtx.() -> Unit): Block {
                    val builder = makeBlockBuilder(label, this)
                    builder.build()
                    val block = builder.run()
                    blocks.add(block)
                    return block
                }

            }
            fnBuilder.build()
            val fn = Fn(
                name = name,
                returnType = returnType ?: Type.Tuple(emptyList()),
                parameters = params,
                entry = entry ?: throw IllegalStateException("entry must be set"),
                blocks = blocks
            )
            items.add(fn)
            return fn
        }
    }
    ctx.build()
    return module
}

private interface BlockBuilderCtxImpl: BlockBuilderCtx {
    fun run(): Block
}

private fun makeBlockBuilder(label: String,  fnBuilderCtx: FnBuilderCtx): BlockBuilderCtxImpl {
    val instructions = mutableListOf<Instruction>()
    var terminator: Terminator? = null
    val ctx = object: BlockBuilderCtxImpl, FnBuilderCtx by fnBuilderCtx {
        override fun emitReturn(value: Value) {
            require(terminator == null) {
                "terminator must be set only once; Previous value: $terminator; New value: Return($value)"
            }
            terminator = Terminator.Return(value)
        }

        override fun run(): Block {
            return Block(
                label = label,
                instructions = instructions,
                terminator = terminator ?: throw IllegalStateException("terminator must be set")
            )
        }
    }
    return ctx
}