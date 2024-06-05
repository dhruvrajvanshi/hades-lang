package hadesboot.middle.lower

import hadesboot.middle.*

fun lowerToC(module: Module) {
    val mirToC = MIRToC()
    val declarations = mirToC.visitModule(module)
    TODO()
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
        val parameters = item.parameters.map { lowerType(it.type) }

        var previousFn = currentFn
        currentFn = mutableListOf()

        visitBlock(item.entry)
        item.blocks.forEach { visitBlock(it) }

        currentFn = previousFn

        declarations.add(CNode.ExternFn(item.name, returnType, parameters))
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


sealed interface CType {
    data object Void: CType
    data class Int(val signed: Boolean, val size: CIntSize): CType
}

sealed interface CNode {
    data class ExternFn(val name: String, val returnType: CType, val parameters: List<CType>): CDecl
    data class Fn(val name: String, val returnType: CType, val parameters: List<Pair<String, CType>>, val body: Block): CDecl

    data class Block(val stmts: List<CStmt>): CStmt
    data class Return(val value: CExpr?): CStmt
    data class Label(val label: String): CStmt

}
sealed interface CDecl: CNode
sealed interface CExpr: CNode {
    data class Int(val value: ULong): CExpr
}
sealed interface CStmt: CNode

enum class CIntSize {
    SIZE,
    I8,
    I16,
    I32,
    I64,
}
fun Type.Width.toCIntSize(): CIntSize = when(this) {
    Type.Width.Size -> CIntSize.SIZE
    Type.Width.W32 -> CIntSize.I32
}