package hadesboot.middle

data class Fn(
    val name: String,
    val returnType: Type,
    val parameters: List<Parameter>,
    val body: Block
)

data class Block(
    val label: String,
    val instructions: List<Instruction>
)

sealed interface Instruction
data class Parameter(val name: String, val type: Type)
