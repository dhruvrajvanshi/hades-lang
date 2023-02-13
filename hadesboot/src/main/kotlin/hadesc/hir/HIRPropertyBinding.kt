package hadesc.hir

import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

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

    fun prettyPrint(): String = when (this) {
        is GlobalExtensionRef -> functionName.mangle()
        is ImplementationMethodRef -> "${implName.mangle()}->$interfaceMemberIndex"
    }
}
