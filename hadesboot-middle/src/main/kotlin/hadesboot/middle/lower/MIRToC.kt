package hadesboot.middle.lower

import hadesboot.middle.*
import hadesboot.prettyprint.PPNode
import hadesboot.prettyprint.PPNode.*
import hadesboot.prettyprint.prettyPrint

fun lowerToC(module: Module): String {
    val mirToC = MIRToC()
    val declarations = mirToC.visitModule(module)

    return declarations.toPPNode().prettyPrint()
}

private class MIRToC {
    private val declarations: MutableList<CDecl> = mutableListOf()
    private var currentFn: MutableList<CNode.Block>? = null
    private var currentBlock: MutableList<CStmt>? = null

    fun visitModule(module: Module): List<CDecl> {
        module.items.forEach { visitItem(it) }
        check(currentFn == null) { "Expected currentFn to be null after visiting all items" }
        check(currentBlock == null) { "Expected block to be null after visiting all items" }
        return declarations
    }

    private fun visitItem(item: Item) {
        when(item) {
            is Fn -> visitFn(item)
            is ExternFn -> visitExternFn(item)
        }
    }

    private fun visitFn(item: Fn) {
        val returnType = lowerType(item.returnType)
        val parameters = item.parameters.map { it.name to lowerType(it.type) }

        val previousFn = currentFn
        currentFn = mutableListOf()

        visitBlock(item.entry)
        item.blocks.forEach { visitBlock(it) }

        declarations.add(CNode.Fn(item.name, returnType, parameters, requireNotNull(currentFn)))
        currentFn = previousFn

    }

    private fun visitBlock(block: Block) {
        val previousBlock = currentBlock
        currentBlock = mutableListOf()

        emitStmt(CNode.Label(block.label))

        block.instructions.forEach { _ -> TODO() }
        visitTerminator(block.terminator)

        emitCurrentBlock()
        currentBlock = previousBlock
    }


    private fun visitTerminator(terminator: Terminator) = when(terminator) {
        is Terminator.Return -> {
            val type = terminator.value.type
            val cType = lowerType(type)
            if (cType == CType.Void) {
                require(terminator.value == Constant.unit) {
                    "The return value of a void function must be Constant.unit"
                }
                emitStmt(CNode.Return(null))
            } else {
                emitStmt(CNode.Return(lowerValue(terminator.value)))
            }
        }
    }
    private fun lowerValue(value: Value): CExpr = when(value) {
        is Constant.Int -> CExpr.Int(value.value)
        is Constant.Tuple -> {
            if (value.members.isEmpty()) {
                throw IllegalStateException("Cannot lower empty tuple as a value; This must be handled by the caller")
            } else {
                TODO("C Lowering of anonymous tuple types")
            }
        }
    }

    private fun emitCurrentBlock() {
        val block = requireNotNull(currentBlock) {
            "emitCurrentBlock must be called within the context of a block"
        }
        emitStmt(CNode.Block(block))
    }

    private fun emitStmt(stmt: CStmt) {
        val block = requireNotNull(currentBlock) {
            "emitStmt must be called within the context of a block"
        }
        block.add(stmt)
    }

    private fun visitExternFn(externFn: ExternFn) {
        val returnType = lowerType(externFn.returnType)
        val parameters = externFn.parameters.map { lowerType(it) }

        declarations.add(CNode.ExternFn(externFn.name, returnType, parameters))
    }

    private fun lowerType(type: Type): CType = when(type) {
        is Type.Int -> CType.Int(type.sign == Type.Sign.Signed, type.width.toCIntSize())
        is Type.Tuple -> {
            if (type.members.isEmpty()) {
                CType.Void
            } else {
                TODO("C Lowering of anonymous tuple types")
            }
        }
    }
}


private sealed interface CType {
    data object Void: CType
    data class Int(val signed: Boolean, val size: CIntSize): CType
}

private sealed interface CNode {
    data class ExternFn(val name: String, val returnType: CType, val parameters: List<CType>): CDecl
    data class Fn(val name: String, val returnType: CType, val parameters: List<Pair<String, CType>>, val blocks: List<CStmt>): CDecl

    data class Block(val stmts: List<CStmt>): CStmt
    data class Return(val value: CExpr?): CStmt
    data class Label(val label: String): CStmt

}
private sealed interface CDecl: CNode
private sealed interface CExpr: CNode {
    data class Int(val value: ULong): CExpr
}
private sealed interface CStmt: CNode

private enum class CIntSize {
    SIZE,
    I8,
    I16,
    I32,
    I64,
}

private fun Type.Width.toCIntSize(): CIntSize = when(this) {
    Type.Width.Size -> CIntSize.SIZE
    Type.Width.W32 -> CIntSize.I32
}

private fun List<CDecl>.toPPNode(): PPNode {
    return Nodes(
        map {
            it.toPPNode() + Text("\n\n")
        }
    )
}

private fun CDecl.toPPNode(): PPNode = when (this) {
    is CNode.ExternFn -> toPPNode()
    is CNode.Fn -> toPPNode()
}
private fun CNode.ExternFn.toPPNode(): PPNode = Group(
    Indent(
        Text("extern"),
        SpaceOrLine,
        returnType.toPPNode(),
        SpaceOrLine,
        Text(name),
        IfWrap(SpaceOrLine, Text("")),
        Group(
            Text("("),
            LineIfWrapping,
            Indent(
                parameters.mapIndexed { index, it ->
                    it.toPPNode() +
                    if (index == parameters.lastIndex) {
                        Text("")
                    } else {
                        Text(",") + SpaceOrLine
                    }
                }
            ),
            LineIfWrapping,
            Text(")"),
            Text(";")
        )
    )
)
private fun CNode.Fn.toPPNode(): PPNode = Group(
    Indent(
        returnType.toPPNode(),
        SpaceOrLine,
        Text(name),
        IfWrap(SpaceOrLine, Text("")),
        Group(
            Text("("),
            LineIfWrapping,
            Indent(
                parameters.mapIndexed { index, it ->
                    it.second.toPPNode() + Text(" ") + Text(it.first) +
                    if (index == parameters.lastIndex) {
                        Text("")
                    } else {
                        Text(",") + SpaceOrLine
                    }
                }
            ),
            LineIfWrapping,
            Text(")"),
            SpaceOrLine,
            Group(
                Text("{"),
                LineIfWrapping,
                Indent(
                    blocks.map { it.toPPNode() + Text("\n\n") }
                ),
                LineIfWrapping,
                Text("}")
            ).forceWrap()
        )
    )
)

private fun CNode.Block.toPPNode(): PPNode = Group(
    Text("{"),
    LineIfWrapping,
    Indent(stmts.map { it.toPPNode() + LineIfWrapping }),
    LineIfWrapping,
    Text("}"),
)

private fun CStmt.toPPNode(): PPNode = when(this) {
    is CNode.Return -> toPPNode()
    is CNode.Label -> toPPNode()
    is CNode.Block -> toPPNode()
}


private fun CNode.Return.toPPNode(): PPNode = Group(
    Text("return"),
    SpaceOrLine,
    Indent(
        value?.toPPNode() ?: Text(""),
    ),
    Text(";")
)
private fun CNode.Label.toPPNode(): PPNode = Text("$label:")

private fun CExpr.toPPNode(): PPNode = when(this) {
    is CExpr.Int -> Text(value.toString())
}

private fun CType.toPPNode(): PPNode = when(this) {
    is CType.Void -> Text("void")
    is CType.Int -> Text("${if (signed) "" else "u"}int${size.str()}_t")
}
private fun CIntSize.str() = when(this) {
    CIntSize.SIZE -> "size"
    CIntSize.I8 -> "8"
    CIntSize.I16 -> "16"
    CIntSize.I32 -> "32"
    CIntSize.I64 -> "64"
}