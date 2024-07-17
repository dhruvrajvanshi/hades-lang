package hadesc.hir

import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed interface HIRConstant : HIROperand {

    data class CString(
        override val location: SourceLocation,
        override val type: Type,
        val text: String
    ) : HIRConstant

    data class BoolValue(
        override val location: SourceLocation,
        override val type: Type,
        val value: Boolean
    ) : HIRConstant

    data class IntValue(
        override val location: SourceLocation,
        override val type: Type,
        val value: Int
    ) : HIRConstant

    data class FloatValue(
        override val location: SourceLocation,
        override val type: Type.FloatingPoint,
        val value: Double
    ) : HIRConstant

    data class Void(override val location: SourceLocation) : HIRConstant {
        override val type: Type
            get() = Type.Void
    }

    data class SizeOf(
        override val location: SourceLocation,
        override val type: Type,
        val ofType: Type
    ) : HIRConstant

    data class AlignOf(
        override val location: SourceLocation,
        override val type: Type,
        val ofType: Type
    ) : HIRConstant

    data class NullPtr(
        override val location: SourceLocation,
        override val type: Type.Ptr
    ) : HIRConstant

    data class StructValue(
        override val location: SourceLocation,
        override val type: Type,
        val values: List<HIRConstant>
    ) : HIRConstant

    data class GlobalFunctionRef(
        override val location: SourceLocation,
        override val type: Type,
        val name: QualifiedName
    ) : HIRConstant

    data class Error(
        override val location: SourceLocation,
        override val type: Type,
        val message: String
    ): HIRConstant

}
