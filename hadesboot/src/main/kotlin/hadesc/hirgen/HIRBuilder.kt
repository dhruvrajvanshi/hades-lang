package hadesc.hirgen

import hadesc.Name
import hadesc.context.NamingContext
import hadesc.hir.HIRBlock
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.location.SourceLocation
import hadesc.types.Type

internal interface HIRBuilder {
    var currentLocation: SourceLocation
    val namingCtx: NamingContext
    var currentStatements: MutableList<HIRStatement>?
    fun buildBlock(location: SourceLocation, name: Name? = null, builder: () -> Unit): HIRBlock

    fun HIRExpression.getStructField(name: Name, index: Int, type: Type): HIRExpression.GetStructField {
        return HIRExpression.GetStructField(
            currentLocation,
            type,
            this,
            name,
            index
        )
    }

    fun HIRExpression.getStructField(name: String, index: Int, type: Type): HIRExpression.GetStructField {
        return getStructField(namingCtx.makeName(name), index, type)
    }

    fun addressOf(valRef: HIRExpression.ValRef): HIRExpression.AddressOf {
        return HIRExpression.AddressOf(
            currentLocation,
            Type.Ptr(valRef.type, isMutable = false),
            valRef.name
        )
    }

    fun HIRExpression.fieldPtr(name: Name, index: Int, type: Type.Ptr): HIRExpression.GetStructFieldPointer {
        return HIRExpression.GetStructFieldPointer(
            currentLocation,
            type,
            this,
            name,
            index
        )
    }

    fun HIRExpression.ptrCast(toPointerOfType: Type): HIRExpression.PointerCast {
        return HIRExpression.PointerCast(
            currentLocation,
            toPointerOfType,
            this
        )
    }

    fun HIRExpression.deref(): HIRExpression {
        val ptrTy = type
        check(ptrTy is Type.Ptr)

        return HIRExpression.Load(currentLocation, ptrTy.to, this)
    }
}


internal fun <T: HIRStatement> HIRBuilder.emit(statement: T): T {
    requireNotNull(currentStatements).add(statement)
    return statement
}

internal fun HIRBuilder.declareVariable(namePrefix: String = "", type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef {
    val name = namingCtx.makeUniqueName(namePrefix)
    return declareVariable(name, type, location)
}
internal fun HIRBuilder.declareVariable(name: Name, type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef {
    emit(HIRStatement.ValDeclaration(
        location,
        name,
        type = type,
        isMutable = false)
    )

    return HIRExpression.ValRef(
        location,
        type,
        name,
    )
}


internal fun HIRBuilder.declareAndAssign(namePrefix: String = "", rhs: HIRExpression, location: SourceLocation = rhs.location): HIRExpression.ValRef {
    val variable = declareVariable(namePrefix, rhs.type, location)
    emit(HIRStatement.Assignment(
        location,
        variable.name,
        rhs
    ))

    return variable
}

internal fun HIRBuilder.emitAssign(valRef: HIRExpression.ValRef, rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Assignment =
    emit(
        HIRStatement.Assignment(
            location,
            valRef.name,
            rhs
        )
    )
