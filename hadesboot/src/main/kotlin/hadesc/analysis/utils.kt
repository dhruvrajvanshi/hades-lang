package hadesc.analysis

import hadesc.ast.HasDefId
import hadesc.ast.HasTypeParams
import hadesc.types.Type


internal fun <Def> Def.makeTypeParams(): List<Type.Param>
        where Def: HasDefId, Def: HasTypeParams =
    typeParams.let { it ?: emptyList() }.mapIndexed { index, it ->
        Type.Param(it.binder)
    }