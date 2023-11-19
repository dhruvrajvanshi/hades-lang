package hadesc.context

import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.types.Type
import java.nio.file.Path

interface GlobalConstantsCtx {
    val builtinSourceLocation: SourceLocation
    val enumTagType: Type
}

internal class GlobalConstantsCtxImpl: GlobalConstantsCtx {
    override val enumTagType: Type = Type.u8
    override val builtinSourceLocation: SourceLocation = SourceLocation(
        SourcePath(Path.of("builtin")),
        start = Position(0, 0),
        stop = Position(0, 0),
    )
}