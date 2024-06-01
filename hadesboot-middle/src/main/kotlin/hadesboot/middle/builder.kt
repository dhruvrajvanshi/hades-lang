package hadesboot.middle

interface TypeBuilderCtx {
    val i32 get() = Type.I32
    val u32 get() = Type.U32
    val isize get() = Type.ISize
    val usize get() = Type.USize
    fun tuple(vararg of: Type): Type = Type.Tuple(of.toList())
}

interface ModuleBuilderCtx: TypeBuilderCtx {
    fun addFn(name: String, build: FnBuilderCtx.() -> Unit): Fn
}

interface FnBuilderCtx: ModuleBuilderCtx {
    fun param(name: String, type: Type): Parameter
    fun returns(type: Type)
}

fun buildModule(name: String, build: ModuleBuilderCtx.() -> Unit): Module {
    val items = mutableListOf<Item>()
    val module = Module(name = name, items = items)
    val ctx = object : ModuleBuilderCtx {
        override fun addFn(name: String, build: FnBuilderCtx.() -> Unit): Fn {
            val params = mutableListOf<Parameter>()
            var returnType: Type? = null
            val entry = Block("entry", emptyList(), terminator = Terminator.Return)
            val fnBuilder = object : FnBuilderCtx,  ModuleBuilderCtx by this {
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
            }
            fnBuilder.build()
            val fn = Fn(
                name = name,
                returnType = returnType ?: Type.Tuple(emptyList()),
                parameters = params,
                entry = entry
            )
            return fn
        }
    }
    ctx.build()
    return module
}