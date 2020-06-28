package hadesc.hir

import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class HIRPropertyBinding : HasLocation {

    data class GlobalExtensionRef(
            override val location: SourceLocation,
            val functionName: QualifiedName
    ) : HIRPropertyBinding()

    data class ImplementationMethodRef(
            override val location: SourceLocation,
            val implName: QualifiedName,
            val interfaceMemberIndex: Int
    ) : HIRPropertyBinding()
}