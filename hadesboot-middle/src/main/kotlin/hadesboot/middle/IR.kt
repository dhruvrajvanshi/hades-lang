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
    val body: Block
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
sealed interface Terminator
data class Parameter(val name: String, val type: Type)
