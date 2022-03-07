package hadesc.hirgen

import hadesc.Name
import hadesc.context.NamingContext
import hadesc.hir.HIRBlock
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.location.SourceLocation
import hadesc.types.Type

internal interface HIRBuilder {
    val currentLocation: SourceLocation
    val namingCtx: NamingContext
    var currentStatements: MutableList<HIRStatement>?
    fun buildBlock(location: SourceLocation, name: Name? = null, builder: () -> Unit): HIRBlock
}


internal fun <T: HIRStatement> HIRBuilder.emit(statement: T): T {
    requireNotNull(currentStatements).add(statement)
    return statement
}

internal fun HIRBuilder.declareVariable(namePrefix: String = "", type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef {
    val name = namingCtx.makeUniqueName(namePrefix)
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


internal fun HIRBuilder.declareAndAssign(namePrefix: String = "", rhs: HIRExpression, location: SourceLocation = rhs.location): HIRExpression {
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
