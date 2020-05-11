package hadesc.ir

import hadesc.Name
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

class IRTypeParam(val name: IRLocalName, val binderLocation: SourceLocation)

class IRBlock(val name: IRLocalName = IRLocalName(Name("entry"))) {
    var statements = mutableListOf<IRInstruction>()
    fun prettyPrint(): String =
        "\n${name.prettyPrint()}:\n${statementSequence().joinToString("\n") { "  " + it.prettyPrint() }}\n"

    operator fun iterator(): Iterator<IRInstruction> = statementSequence().iterator()

    private fun statementSequence() = statements.toList().asSequence()
}

