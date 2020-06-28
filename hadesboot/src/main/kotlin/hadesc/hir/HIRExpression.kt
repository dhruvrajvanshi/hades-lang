package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class HIRExpression: HasLocation {
    abstract val type: Type
    data class Call(
            override val location: SourceLocation,
            override val type: Type,
            val callee: HIRExpression,
            val typeArgs: List<Type>?,
            val args: List<HIRExpression>
    ) : HIRExpression()

    data class GlobalRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: QualifiedName
    ) : HIRExpression()

    data class Constant(
            val constant: HIRConstant
    ) : HIRExpression() {
        override val location: SourceLocation
            get() = constant.location

        override val type: Type
            get() = constant.type
    }

    data class ParamRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: Name
    ) : HIRExpression()

    data class ValRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: Name
    ) : HIRExpression()

    data class GetStructField(
            override val location: SourceLocation,
            override val type: Type,
            val lhs: HIRExpression,
            val name: Name,
            val index: Int
    ) : HIRExpression()

    data class ThisRef(
            override val location: SourceLocation,
            override val type: Type
    ) : HIRExpression()

    data class MethodRef(
            override val location: SourceLocation,
            override val type: Type,
            val thisValue: HIRExpression,
            val propertyBinding: HIRPropertyBinding
    ) : HIRExpression()
}