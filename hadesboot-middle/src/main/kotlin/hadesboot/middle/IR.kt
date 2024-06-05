package hadesboot.middle

data class Module(
    val name: String,
    val items: List<Item>,
)

sealed interface Item

data class Fn(
    val name: String,
    val returnType: Type,
    val parameters: List<Parameter>,
    val entry: Block,
    val blocks: List<Block>,
): Item

data class ExternFn(
    val name: String,
    val returnType: Type,
    val parameters: List<Type>,
): Item

data class Block(
    val label: String,
    val instructions: List<Instruction>,
    val terminator: Terminator,
)

sealed interface Instruction
sealed interface Terminator {
    data class Return(val value: Value): Terminator
}
data class Parameter(val name: String, val type: Type)

sealed interface Value {
    val type: Type
}
sealed interface Constant: Value {
    data class Int(override val type: Type.Int, val value: ULong): Constant
    data class Tuple(val members: List<Value>): Constant {
        override val type: Type get() = Type.Tuple(members.map { it.type })
    }

    companion object {
        fun tuple(vararg members: Value) = Tuple(members.toList())
        val unit = Tuple(emptyList())
    }
}
