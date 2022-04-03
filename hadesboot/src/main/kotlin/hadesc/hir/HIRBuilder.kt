package hadesc.hir

import hadesc.Name
import hadesc.context.NamingContext
import hadesc.location.SourceLocation
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr
import hadesc.types.toSubstitution

interface HIRBuilder {
    var currentLocation: SourceLocation
    val namingCtx: NamingContext
    var currentStatements: MutableList<HIRStatement>?

    fun HIRExpression.getStructField(name: Name, index: Int, type: Type): HIRExpression.LocalRef {
        val s = emit(HIRStatement.GetStructField(location, namingCtx.makeUniqueName(), type, this, name, index))
        return HIRExpression.LocalRef(location, type, s.name)
    }

    fun HIRStatement.Alloca.ptr(location: SourceLocation = currentLocation): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type.ptr(), name)
    }

    fun HIRStatement.BinOp.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type, name)
    }

    fun HIRStatement.Alloca.mutPtr(location: SourceLocation = currentLocation): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type.mutPtr(), name)
    }

    fun HIRExpression.getStructField(name: String, index: Int, type: Type): HIRExpression.LocalRef {
        return getStructField(namingCtx.makeName(name), index, type)
    }

    fun HIRStatement.Call.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            resultType,
            name
        )
    }

    fun HIRStatement.TypeApplication.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            type,
            name
        )
    }

    fun addressOf(valRef: HIRExpression.ValRef): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            valRef.type.ptr(),
            valRef.name
        )
    }

    fun HIRExpression.fieldPtr(name: Name, index: Int, type: Type.Ptr): HIRExpression.LocalRef {
        val s = emit(HIRStatement.GetStructFieldPointer(location, namingCtx.makeUniqueName(), type, this, name, index))
        return HIRExpression.LocalRef(
            location,
            type,
            s.name,
        )
    }

    fun HIRExpression.fieldPtr(name: String, index: Int, type: Type.Ptr): HIRExpression.LocalRef {
        return fieldPtr(namingCtx.makeName(name), index, type)
    }

    fun HIRExpression.ptrCast(toPointerOfType: Type): HIRExpression.LocalRef {
        val s = emit(HIRStatement.PointerCast(
            currentLocation,
            namingCtx.makeUniqueName(),
            toPointerOfType,
            this
        ))
        return HIRExpression.LocalRef(
            currentLocation,
            s.type,
            s.name
        )
    }

    fun HIRExpression.load(): HIRExpression.LocalRef {
        val ptrTy = type
        check(ptrTy is Type.Ptr)

        return if (this is HIROperand) {
            val name = namingCtx.makeUniqueName()
            emit(HIRStatement.Load(currentLocation, name, this))
            HIRExpression.LocalRef(currentLocation, ptrTy.to, name)
        } else {
            val ptrRef = allocaAssign(namingCtx.makeUniqueName(), this)
            ptrRef.ptr().load().load()
        }

    }
}


fun <T: HIRStatement> HIRBuilder.emit(statement: T): T {
    requireNotNull(currentStatements).add(statement)
    return statement
}

fun HIRBuilder.emitAll(statements: Iterable<HIRStatement>) {
    requireNotNull(currentStatements).addAll(statements)
}

fun HIRBuilder.emitIntegerConvert(expression: HIRExpression, to: Type): HIROperand {
    val s = emit(HIRStatement.IntegerConvert(
        expression.location,
        namingCtx.makeUniqueName(),
        to,
        expression
    ))
    return HIRExpression.LocalRef(
        s.location,
        s.type,
        s.name
    )
}

fun HIRBuilder.emitTypeApplication(lhs: HIROperand, args: List<Type>): HIRStatement.TypeApplication {
    val lhsType = lhs.type
    check(lhsType is Type.TypeFunction)
    check(lhsType.params.size == args.size)
    val appliedType = lhsType.body.applySubstitution(
        lhsType.params.zip(args).toSubstitution()
    )
    return emit(HIRStatement.TypeApplication(currentLocation, namingCtx.makeUniqueName(), appliedType, lhs, args))
}

@Deprecated("Use emitAlloca")
fun HIRBuilder.declareVariable(namePrefix: String = "", type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef {
    val name = namingCtx.makeUniqueName(namePrefix)
    val alloca = emitAlloca(name, type, location)
    return HIRExpression.ValRef(location, type, alloca.name)
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

@Deprecated(replaceWith = ReplaceWith("emitStore"), message = "Refactor to use emit store")
fun HIRBuilder.emitAssign(valRef: HIRExpression.ValRef, rhs: HIRExpression, location: SourceLocation = rhs.location) =
    emitAssign(valRef.name, rhs, location)

@Deprecated(replaceWith = ReplaceWith("emitStore"), message = "Refactor to store instructions")
fun HIRBuilder.emitAssign(name: Name, rhs: HIRExpression, location: SourceLocation = rhs.location) {
    emit(
        HIRStatement.Assignment(
            location,
            name,
            rhs
        )
    )
}

fun HIRBuilder.emitStore(ptr: HIROperand, value: HIRExpression) {
    val ptrType = ptr.type
    check(ptrType is Type.Ptr && ptrType.isMutable)
    emit(HIRStatement.Store(value.location, ptr, value))
}

fun HIRBuilder.emitAlloca(name: Name, type: Type, location: SourceLocation = currentLocation): HIRStatement.Alloca {
    return emit(HIRStatement.Alloca(location, name, isMutable = true, type))
}

fun HIRBuilder.emitCall(resultType: Type, callee: HIROperand, args: List<HIRExpression>, location: SourceLocation = currentLocation): HIRStatement.Call {
    val name = namingCtx.makeUniqueName()
    return emit(HIRStatement.Call(location, resultType, name, callee, args))
}

fun HIRBuilder.emitAlloca(namePrefix: String, type: Type, location: SourceLocation = currentLocation): HIRStatement.Alloca {
    return emitAlloca(namingCtx.makeUniqueName(namePrefix), type, location)
}

fun HIRBuilder.buildBlock(location: SourceLocation = currentLocation, name: Name? = null, builder: () -> Unit): HIRBlock {
    val statements = mutableListOf<HIRStatement>()
    intoStatementList(statements) { builder() }
    return HIRBlock(location, name ?: namingCtx.makeUniqueName(), statements)
}

fun HIRBuilder.intoStatementList(statements: MutableList<HIRStatement>, builder: () -> Unit) {
    val oldStatements = currentStatements
    currentStatements = statements
    builder()
    currentStatements = oldStatements
}

fun HIRBuilder.trueValue(location: SourceLocation = currentLocation): HIRConstant.BoolValue {
    return HIRConstant.BoolValue(location, Type.Bool, true)
}

fun HIRBuilder.falseValue(location: SourceLocation = currentLocation): HIRConstant.BoolValue {
    return HIRConstant.BoolValue(location, Type.Bool, false)
}
