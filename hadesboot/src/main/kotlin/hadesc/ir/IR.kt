package hadesc.ir

import hadesc.Name
import hadesc.ast.Binder
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRBinding {
    abstract val type: Type

    class FunctionDef(val def: IRFunctionDef) : IRBinding() {
        override val type: Type
            get() = def.type

    }

    class ExternFunctionDef(val def: IRExternFunctionDef) : IRBinding() {
        override val type: Type
            get() = def.type
    }

    class StructDef(val def: IRStructDef) : IRBinding() {
        override val type: Type
            get() = def.constructorType

    }

    class ConstDef(val def: IRConstDef) : IRBinding() {
        override val type: Type get() = def.type
    }
}

data class IRTypeParam(
        val name: IRLocalName,
        val binder: Binder
) {
    val binderLocation get() = binder.location
}

class IRBlock(val location: SourceLocation, val name: IRLocalName = IRLocalName(Name("entry"))) {
    var statements = mutableListOf<IRInstruction>()
    var deferBlockName: IRLocalName? = null
    fun prettyPrint(): String =
        "\n${name.prettyPrint()} (defer: ${deferBlockName?.prettyPrint()}):\n${statementSequence().joinToString("\n") { "  " + it.prettyPrint() }}\n"

    operator fun iterator(): Iterator<IRInstruction> = statementSequence().iterator()

    private fun statementSequence() = statements.asSequence()
    fun hasTerminator(): Boolean = statements.isNotEmpty() && statements.last().isTerminator()
}

