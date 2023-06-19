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
        I32 -> "int"
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
    }

    data class FunctionDeclaration(val name: CName, val params: List<CParam>, val returnType: CType): CNode

    data class FunctionDefinition(val name: CName, val params: List<CParam>, val returnType: CType, val body: CBlock): CNode
}

private sealed interface CStatement: CNode {
    data class Return(val expr: CExpr): CStatement
}
private sealed interface CExpr {
    fun prettyPrint(): String = when(this) {
        is IntLiteral -> value.toString()
    }

    data class IntLiteral(val value: Int): CExpr
}

class EmitC(private val root: MIRValue.Object, private val outputFile: Path) {
    private val nodes = mutableListOf<CNode>()

    fun run() {
        // Forward declarations of functions
        for ((name, value) in root.values.entries) {
            when (value) {
                is MIRValue.Function ->
                    nodes.add(
                        CNode.FunctionDeclaration(
                            name.mangle(),
                            returnType = value.returnType.toCType(),
                            params = value.params.map { CParam(it.name.mangle(), it.type.toCType()) })
                    )
                is MIRValue.I32 -> TODO()
                is MIRValue.Object -> TODO()
                is MIRValue.LocalRef -> TODO()
            }
        }

        for ((name, value) in root.values.entries) {
            when (value) {
                is MIRValue.Function ->
                    nodes.add(
                        CNode.FunctionDefinition(
                            name.mangle(),
                            returnType = value.returnType.toCType(),
                            params = value.params.map { CParam(it.name.mangle(), it.type.toCType()) },
                            body = lowerFunctionBody(value.basicBlocks)
                        ),

                    )

                is MIRValue.I32 -> TODO()
                is MIRValue.Object -> TODO()
                is MIRValue.LocalRef -> TODO()
            }
        }
        val text = nodes.joinToString("\n") { it.prettyPrint("") }
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
                is MIRInstruction.IAdd -> TODO()
            }
        }

        into.add(CBlock(block.name, myInstructions))
    }

    private fun MIRValue.toCExpr(): CExpr = when (this) {
        is MIRValue.Function -> TODO()
        is MIRValue.I32 -> CExpr.IntLiteral(value)
        is MIRValue.Object -> TODO()
        is MIRValue.LocalRef -> TODO()
    }


    private fun MIRType.toCType(): CType = when (this) {
        is MIRType.Function -> TODO()
        MIRType.I32 -> CType.I32
        is MIRType.Interface -> TODO()
    }
}

fun MIRValue.Object.emitC(outputPath: Path) {
    EmitC(this, outputPath).run()
}