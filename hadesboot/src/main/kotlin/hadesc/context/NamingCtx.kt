package hadesc.context

import hadesc.Name
import hadesc.qualifiedname.QualifiedName

interface NamingCtx {
    fun makeUniqueName(prefix: String = ""): Name
    fun makeName(text: String): Name
    fun qn(vararg names: String) = QualifiedName(names.map { makeName(it) })
}

class NamingCtxImpl: NamingCtx {
    private var _nameIndex = 0
    override fun makeName(text: String): Name = Name(text)
    override fun makeUniqueName(prefix: String): Name {
        _nameIndex++
        if (prefix.isNotBlank()) {
            return makeName("\$$prefix\$$_nameIndex")
        }
        return makeName("\$$_nameIndex")
    }
}
