package hadesc.ir

import hadesc.Name
import hadesc.qualifiedname.QualifiedName

sealed class IRName {
    fun prettyPrint() = when (this) {
        is IRLocalName -> "%${name.text}"
        is IRGlobalName -> "@${name.mangle()}"
    }

    fun mangle() = when (this) {
        is IRLocalName -> name.text
        is IRGlobalName -> name.mangle()
    }

}

data class IRLocalName(val name: Name) : IRName()
data class IRGlobalName(val name: QualifiedName) : IRName()