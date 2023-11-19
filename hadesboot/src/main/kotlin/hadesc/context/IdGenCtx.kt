package hadesc.context

import hadesc.BinderId
import hadesc.ast.DefId
import hadesc.ast.SourceFileId

interface IdGenCtx {
    fun makeBinderId(): BinderId
    fun makeDefId(): DefId
    fun makeSourceFileId(): SourceFileId
}

internal class IdGenCtxImpl: IdGenCtx {
    private var nextDefId = 0
    private var nextSourceFileId = 0
    private var nextBinderId = 0U

    override fun makeDefId(): DefId = DefId(nextDefId).also { nextDefId++ }

    override fun makeSourceFileId() = SourceFileId(nextSourceFileId).also { nextSourceFileId++ }

    override fun makeBinderId() = BinderId(nextBinderId).also { nextBinderId++ }
}