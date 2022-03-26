package hadesc.hir

import hadesc.Name
import hadesc.context.NamingContext
import hadesc.location.SourceLocation
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr

interface HIRBuilder {
    var currentLocation: SourceLocation
    val namingCtx: NamingContext
    var currentStatements: MutableList<HIRStatement>?

    fun HIRExpression.getStructField(name: Name, index: Int, type: Type): HIRExpression.GetStructField {
        return HIRExpression.GetStructField(
            currentLocation,
            type,
            this,
            name,
            index
        )
    }

    fun HIRStatement.Alloca.ptr(location: SourceLocation = currentLocation): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type.ptr(), name)
    }

    fun HIRStatement.Alloca.mutPtr(location: SourceLocation = currentLocation): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type.mutPtr(), name)
    }

    fun HIRExpression.getStructField(name: String, index: Int, type: Type): HIRExpression.GetStructField {
        return getStructField(namingCtx.makeName(name), index, type)
    }

    fun addressOf(valRef: HIRExpression.ValRef): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            valRef.type.ptr(),
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

    fun HIRExpression.fieldPtr(name: String, index: Int, type: Type.Ptr): HIRExpression.GetStructFieldPointer {
        return fieldPtr(namingCtx.makeName(name), index, type)
    }

    fun HIRExpression.ptrCast(toPointerOfType: Type): HIRExpression.PointerCast {
        return HIRExpression.PointerCast(
            currentLocation,
            toPointerOfType,
            this
        )
    }

    fun HIRExpression.load(): HIRExpression {
        val ptrTy = type
        check(ptrTy is Type.Ptr)

        return HIRExpression.Load(currentLocation, ptrTy.to, this)
    }
}


fun <T: HIRStatement> HIRBuilder.emit(statement: T): T {
    requireNotNull(currentStatements).add(statement)
    return statement
}

fun HIRBuilder.emitAll(statements: Iterable<HIRStatement>) {
    requireNotNull(currentStatements).addAll(statements)
}

fun HIRBuilder.declareVariable(namePrefix: String = "", type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef {
    val name = namingCtx.makeUniqueName(namePrefix)
    return declareVariable(name, type, location)
}
fun HIRBuilder.declareVariable(name: Name, type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef {
    emit(HIRStatement.Alloca(
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


fun HIRBuilder.allocaAssign(namePrefix: String = "", rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Alloca {
    val name = namingCtx.makeUniqueName(namePrefix)
    return allocaAssign(name, rhs, location)
}

fun HIRBuilder.allocaAssign(name: Name, rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Alloca {
    val alloca = emitAlloca(name, rhs.type, location)
    emitStore(alloca.mutPtr(), rhs)
    return alloca
}

fun HIRBuilder.emitAssign(valRef: HIRExpression.ValRef, rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Assignment =
    emitAssign(valRef.name, rhs, location)

fun HIRBuilder.emitAssign(name: Name, rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Assignment =
    emit(
        HIRStatement.Assignment(
            location,
            name,
            rhs
        )
    )

fun HIRBuilder.emitStore(ptr: HIRExpression, value: HIRExpression) {
    val ptrType = ptr.type
    check(ptrType is Type.Ptr && ptrType.isMutable)
    emit(HIRStatement.Store(value.location, ptr, value))
}

fun HIRBuilder.emitAlloca(name: Name, type: Type, location: SourceLocation = currentLocation): HIRStatement.Alloca {
    return emit(HIRStatement.Alloca(location, name, isMutable = true, type))
}

fun HIRBuilder.buildBlock(location: SourceLocation = currentLocation, name: Name? = null, builder: () -> Unit): HIRBlock {
    val oldStatements = currentStatements
    val statements = mutableListOf<HIRStatement>()
    currentStatements = statements
    builder()
    currentStatements = oldStatements
    return HIRBlock(location, name ?: namingCtx.makeUniqueName(), statements)
}

fun HIRBuilder.trueValue(location: SourceLocation = currentLocation): HIRConstant.BoolValue {
    return HIRConstant.BoolValue(location, Type.Bool, true)
}

fun HIRBuilder.falseValue(location: SourceLocation = currentLocation): HIRConstant.BoolValue {
    return HIRConstant.BoolValue(location, Type.Bool, false)
}
