package hadesc.hir

import hadesc.Name
import hadesc.analysis.TypeAnalyzer
import hadesc.context.NamingContext
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr
import hadesc.types.toSubstitution

interface HIRBuilder {
    var currentLocation: SourceLocation
    val namingCtx: NamingContext
    var currentStatements: MutableList<HIRStatement>?
    val currentModule: HIRModule
    val typeAnalyzer: TypeAnalyzer

    fun HIRExpression.getStructField(name: Name, resultName: Name = namingCtx.makeUniqueName()): HIRExpression.LocalRef {
        val lhsType = this.type
        check(lhsType !is Type.Ptr)
        val structName = lhsType.nominalName()
        val structDef = currentModule.findStructDef(structName)
        val fieldIndex = structDef.fieldIndex(name)
        val typeArgs = lhsType.typeArgs()
        val fieldType = structDef.fieldType(name).applyTypeArgs(structDef.typeParams, typeArgs)
        val s =
            emit(HIRStatement.GetStructField(location, resultName, fieldType, this, name, fieldIndex))
        return HIRExpression.LocalRef(location, fieldType, s.name)
    }

    fun HIRStatement.Alloca.ptr(location: SourceLocation = currentLocation): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type.ptr(), name)
    }

    fun HIRStatement.BinOp.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type, name)
    }

    fun Type.verifyAssignableTo(destination: Type) {
        if (this is Type.Select || destination is Type.Select) {
            // FIXME: Right now, type analyzer doesn't have access to
            //        trait bodies, so it can't reduce Trait[someType].AssociatedType
            //        to it's substituted type.
            return
        }
        check(typeAnalyzer.isTypeAssignableTo(source = this, destination = destination)) {
            "${this.prettyPrint()} is not assignable to ${destination.prettyPrint()}"
        }
    }

    fun HIRStatement.Alloca.mutPtr(location: SourceLocation = currentLocation): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(location, type.mutPtr(), name)
    }

    fun HIRExpression.getStructField(name: String): HIRExpression.LocalRef {
        return getStructField(namingCtx.makeName(name))
    }

    fun HIRStatement.Call.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            resultType,
            name
        )
    }

    fun HIRStatement.InvokeClosure.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            type,
            name
        )
    }

    fun HIRStatement.AllocateClosure.result(): HIRExpression.LocalRef {
        return HIRExpression.LocalRef(
            currentLocation,
            type,
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

    fun HIRDefinition.Const.ref(): HIRExpression {
        return HIRExpression.GlobalRef(
            currentLocation,
            type,
            name
        )
    }

    fun HIRExpression.fieldPtr(name: Name, resultName: Name = namingCtx.makeUniqueName()): HIRExpression.LocalRef {
        val lhsType = this.type
        check(lhsType is Type.Ptr)
        val lhsStructType = lhsType.to
        val structName = lhsStructType.nominalName()
        val structDef = currentModule.findStructDef(structName)
        val fieldIndex = structDef.fieldIndex(name)
        val typeArgs = lhsStructType.typeArgs()
        val fieldType = structDef.fieldType(name).applyTypeArgs(structDef.typeParams, typeArgs)
        val s = emit(HIRStatement.GetStructFieldPointer(location, resultName, fieldType.mutPtr(), this, name, fieldIndex))

        return HIRExpression.LocalRef(
            location,
            s.type,
            s.name,
        )
    }

    fun HIRDefinition.Function.ref(): HIRExpression.GlobalRef {
        return HIRExpression.GlobalRef(
            currentLocation,
            fnPtrType,
            name
        )
    }

    fun HIRDefinition.ExternFunction.ref(): HIRExpression.GlobalRef {
        return HIRExpression.GlobalRef(
            currentLocation,
            type.ptr(),
            name
        )
    }

    fun debugDump(value: HIROperand) {
        val stderrVal = emitCall(Type.Void, libhdcFn("get_stderr").ref(), emptyList()).result()
        when (value.type) {
            is Type.Ptr -> {
                emitCall(Type.Void,
                    libhdcFn("file_put_void_ptr").ref(),
                    listOf(
                        stderrVal,
                        value.ptrCast(Type.Void)
                    )
                )
            }
            else -> TODO("Debug dump not implemented for ${value.type.prettyPrint()}")
        }
    }

    private fun libhdcFn(name: String): HIRDefinition.Function {
        return currentModule.findGlobalFunction(qn("hades", "libhdc", "get_stderr"))
    }

    fun HIROperand.typeApplication(args: List<Type>): HIROperand {
        val ty = type
        check(ty is Type.TypeFunction)
        check(ty.params.size == args.size)
        val substitution = ty.params.zip(args).toSubstitution()
        return emit(HIRStatement.TypeApplication(
            currentLocation,
            namingCtx.makeUniqueName(),
            ty.body.applySubstitution(substitution),
            this,
            args
        )).result()
    }

    fun HIRExpression.fieldPtr(name: String): HIRExpression.LocalRef {
        return fieldPtr(namingCtx.makeName(name))
    }

    fun HIRExpression.ptrCast(toPointerOfType: Type, nameHint: String = ""): HIRExpression.LocalRef {
        val s = emit(HIRStatement.PointerCast(
            currentLocation,
            namingCtx.makeUniqueName(nameHint),
            toPointerOfType,
            this
        ))
        return HIRExpression.LocalRef(
            currentLocation,
            s.type,
            s.name
        )
    }

    fun HIRExpression.load(name: Name = namingCtx.makeUniqueName()): HIRExpression.LocalRef {
        val ptrTy = type
        check(ptrTy is Type.Ptr)

        return if (this is HIROperand) {
            emit(HIRStatement.Load(currentLocation, name, this))
            HIRExpression.LocalRef(currentLocation, ptrTy.to, name)
        } else {
            val ptrRef = allocaAssign(name, this)
            ptrRef.ptr().load().load()
        }

    }

    fun HIRParam.ref(): HIRExpression.ParamRef {
        return HIRExpression.ParamRef(
            currentLocation,
            type,
            name,
            binder,
        )
    }
}


fun <T: HIRStatement> HIRBuilder.emit(statement: T): T {
    requireNotNull(currentStatements).add(statement)
    return statement
}

fun <T: HIRDefinition> HIRBuilder.emitDef(definition: T): T {
    currentModule.addDefinition(definition)
    return definition
}

fun HIRBuilder.emitAll(statements: Iterable<HIRStatement>) {
    requireNotNull(currentStatements).addAll(statements)
}

fun HIRBuilder.emitIntegerConvert(expression: HIROperand, to: Type): HIROperand {
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

fun HIRBuilder.allocaAssign(namePrefix: String = "", rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Alloca {
    val name = namingCtx.makeUniqueName(namePrefix)
    return allocaAssign(name, rhs, location)
}

fun HIRBuilder.allocaAssign(name: Name, rhs: HIRExpression, location: SourceLocation = rhs.location): HIRStatement.Alloca {
    val alloca = emitAlloca(name, rhs.type, location)
    emitStore(alloca.mutPtr(), rhs)
    return alloca
}

fun HIRBuilder.emitStore(ptr: HIROperand, value: HIRExpression) {
    val ptrType = ptr.type
    check(ptrType is Type.Ptr && ptrType.isMutable)
    check(
        typeAnalyzer.isTypeAssignableTo(
            source = value.type,
            destination = ptrType.to
        )
    ) {
        "${value.type.prettyPrint()} is not assignable to ${ptrType.to.prettyPrint()}"
    }
    emit(HIRStatement.Store(value.location, ptr, value))
}

fun HIRBuilder.emitAlloca(name: Name, type: Type, location: SourceLocation = currentLocation): HIRStatement.Alloca {
    return emit(HIRStatement.Alloca(location, name, isMutable = true, type))
}

fun HIRBuilder.emitCall(
    resultType: Type,
    callee: HIROperand,
    args: List<HIRExpression>,
    location: SourceLocation = currentLocation,
    /**
     * Temporary way to skip the type checking of arguments
     * for specific cases.
     * TODO: Remove the need for this flag
     *       Currently only used for one case where the callee
     *       type is not a fn ptr when Enum case constructors
     *       without parameters are automatically converted
     *       to no parameter functions. This is currently done
     *       in HIRGen in a hacky way. Once that is fixed,
     *       this flag won't be required.
     */
    skipVerification: Boolean = false,
    name: Name = namingCtx.makeUniqueName()
): HIRStatement.Call {
    val calleeType = callee.type
    if (!skipVerification) {
        check(calleeType is Type.Ptr) {
            "Expected ${calleeType.prettyPrint()} to be a function pointer"
        }
        check(calleeType.to is Type.Function) {
            "Expected ${calleeType.prettyPrint()} to be a function pointer"
        }
        val fnType = calleeType.to
        check(fnType.from.size == args.size) {
            "Wrong number of args to fn of type ${fnType.prettyPrint()}"
        }

        for ((expected, arg) in fnType.from.zip(args)) {
            arg.type.verifyAssignableTo(expected)
        }
        resultType.verifyAssignableTo(fnType.to)

    }
    return emit(HIRStatement.Call(location, resultType, name, callee, args))
}

fun HIRBuilder.emitAlloca(namePrefix: String, type: Type, location: SourceLocation = currentLocation): HIRStatement.Alloca {
    return emitAlloca(namingCtx.makeUniqueName(namePrefix), type, location)
}

fun HIRBuilder.emitReturn(value: HIRExpression): HIRStatement.Return =
    emit(HIRStatement.Return(currentLocation, value))

fun HIRBuilder.buildBlock(
    location: SourceLocation = currentLocation,
    name: Name? = null,
    into: HIRBlock = HIRBlock(location, name ?: namingCtx.makeUniqueName(), mutableListOf()),
    builder: () -> Unit
): HIRBlock {
    val statements = into.statements
    intoStatementList(statements) { builder() }
    return into
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

internal fun HIRBuilder.qn(vararg names: String): QualifiedName {
    return QualifiedName(names.toList().map { namingCtx.makeName(it) })
}

fun HIRBuilder.emitDumpCStr(operand: HIROperand) {
    val puts = currentModule.findDefinition(qn("_hdc_puts"))
    check(puts is HIRDefinition.ExternFunction)

    check(operand.type == Type.u8.ptr() || operand.type == Type.u8.mutPtr())
    emitCall(
        Type.Void,
        puts.ref(),
        listOf(operand)
    )
}

fun HIRBuilder.emitDumpText(string: String) {
    val puts = currentModule.findDefinition(qn("_hdc_puts"))
    check(puts is HIRDefinition.ExternFunction)
    emitCall(
        Type.Void,
        puts.ref(),
        listOf(
            HIRConstant.ByteString(
                currentLocation,
                Type.u8.ptr(),
                string.toByteArray()
            )
        )
    )
}