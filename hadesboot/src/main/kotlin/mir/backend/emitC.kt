package mir.backend

import mir.*
import java.nio.file.Path
import kotlin.io.path.nameWithoutExtension
import kotlin.io.path.writeText

@JvmInline
private value class CName(val text: String) {
    override fun toString(): String = text
}

private fun String.mangle(): CName = CName(this)

private sealed interface CType {
    fun prettyPrint(): String = when(this) {
        I32 -> "int32_t"
        Void -> "void"
    }

    object Void: CType
    object I32: CType
}
private data class CParam(
    val name: CName,
    val type: CType,
)

private data class CBlock(val label: String?, val nodes: List<CNode>): CNode, CStatement

private sealed interface CNode {
    fun prettyPrint(indent: String): CharSequence = when(this) {
        is CBlock -> {
            val label = if (this.label == null) "" else "${this.label}:"
            "$label{\n" +
                    nodes.joinToString("\n") { "$indent  " + it.prettyPrint("$indent  ") } + "\n$indent}"

        }
        is CStatement.Return -> "return ${expr.prettyPrint()};"
        is FunctionDeclaration -> {
            val paramsStr = params.joinToString(",\n  ") { "${it.type.prettyPrint()} ${it.name}" }
            "${returnType.prettyPrint()} ${name}($paramsStr);"
        }
        is FunctionDefinition -> {
            val paramsStr = params.joinToString(",\n  ") { "${it.type.prettyPrint()} ${it.name}" }
            "${returnType.prettyPrint()} ${name}($paramsStr) ${body.prettyPrint("$indent  ")}"
        }

        is CStatement.InitAssign -> "${type.prettyPrint()} ${name.text} = ${value.prettyPrint()};"
        is StaticDefinition -> "${type.prettyPrint()} ${name.text} = ${initializer.prettyPrint()};"
    }

    data class FunctionDeclaration(val name: CName, val params: List<CParam>, val returnType: CType): CNode

    data class FunctionDefinition(val name: CName, val params: List<CParam>, val returnType: CType, val body: CBlock): CNode
    data class StaticDefinition(
        val name: CName,
        val type: CType,
        val initializer: CExpr
    ) : CNode
}

private sealed interface CStatement: CNode {
    data class Return(val expr: CExpr): CStatement
    data class InitAssign(val name: CName, val type: CType, val value: CExpr): CStatement
}
private sealed interface CExpr {
    fun prettyPrint(): String = when(this) {
        is IntLiteral -> value.toString()
        is Add -> "${lhs.prettyPrint()} + ${rhs.prettyPrint()}"
        is Var -> name.text
    }

    data class IntLiteral(val value: Int): CExpr
    data class Add(val lhs: CExpr, val rhs: CExpr): CExpr
    data class Var(val name: CName) : CExpr
}

class EmitC(private val root: MIRModule, private val outputFile: Path) {
    private val nodes = mutableListOf<CNode>()

    fun run() {
        // Forward declarations of functions
        for (declaration in root.declarations) {
            when (declaration) {
                is MIRDeclaration.Function ->
                    nodes.add(
                        CNode.FunctionDeclaration(
                            declaration.name.mangle(),
                            returnType = declaration.returnType.toCType(),
                            params = declaration.params.map { CParam(it.name.mangle(), it.type.toCType()) })
                    )

                is MIRDeclaration.StaticDefinition ->
                    nodes.add(
                        CNode.StaticDefinition(
                            declaration.name.mangle(),
                            declaration.type.toCType(),
                            declaration.initializer.toCExpr(),
                        )
                    )
            }
        }

        for (declaration in root.declarations) {
            when (declaration) {
                is MIRDeclaration.Function ->
                    nodes.add(
                        CNode.FunctionDefinition(
                            declaration.name.mangle(),
                            returnType = declaration.returnType.toCType(),
                            params = declaration.params.map { CParam(it.name.mangle(), it.type.toCType()) },
                            body = lowerFunctionBody(declaration.basicBlocks)
                        ),

                    )
                is MIRDeclaration.StaticDefinition -> Unit
            }
        }
        val text = "#include <stdint.h>\n" + nodes.joinToString("\n") { it.prettyPrint("") }
        val cFile = Path.of(outputFile.parent.toString(), outputFile.nameWithoutExtension + ".c")
        cFile.writeText(text)

        val exitCode = ProcessBuilder()
            .command("clang", "-o", outputFile.toString(), cFile.toString())
            .inheritIO()
            .start()
            .waitFor()
        check(exitCode == 0)
    }

    private fun lowerFunctionBody(blocks: List<MIRBasicBlock>): CBlock {
        val root = mutableListOf<CStatement>()
        for (block in blocks) {
            lowerBlock(block, into = root)
        }

        return CBlock(label = null, root)
    }

    private fun lowerBlock(block: MIRBasicBlock, into: MutableList<CStatement>) {
        val myInstructions = mutableListOf<CStatement>()
        for (instruction in block.instructions) {
            when (instruction) {
                is MIRInstruction.Return -> myInstructions.add(CStatement.Return(instruction.value.toCExpr()))
                is MIRInstruction.IAdd -> {
                    myInstructions.add(
                        CStatement.InitAssign(
                            name = instruction.name.mangle(),
                            type = instruction.type.toCType(),
                            value = CExpr.Add(
                                instruction.lhs.toCExpr(),
                                instruction.rhs.toCExpr(),
                            )
                        )
                    )
                }
            }
        }

        into.add(CBlock(block.name, myInstructions))
    }

    private fun MIRValue.toCExpr(): CExpr = when (this) {
        is MIRValue.I32 -> CExpr.IntLiteral(value)
        is MIRValue.LocalRef -> CExpr.Var(name.mangle())
        is MIRValue.StaticRef -> CExpr.Var(name.mangle())
    }


    private fun MIRType.toCType(): CType = when (this) {
        is MIRType.Function -> TODO()
        MIRType.I32 -> CType.I32
        is MIRType.Interface -> TODO()
    }
}

fun MIRModule.emitC(outputPath: Path) {
    EmitC(this, outputPath).run()
}