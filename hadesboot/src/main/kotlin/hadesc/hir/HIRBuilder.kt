package hadesc.hir

import hadesc.Name
import hadesc.analysis.TypeAnalyzer
import hadesc.context.NamingCtx
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr
import hadesc.types.toSubstitution

interface HIRBuilder {
    var currentLocation: SourceLocation
    val namingCtx: NamingCtx
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
        val s = emit(HIRStatement.GetStructFieldPointer(location, resultName, fieldType.ptr(lhsType.isMutable), this, name, fieldIndex))

        return HIRExpression.LocalRef(
            location,
            s.type,
            s.name
        )
    }

    fun Type.getStructDef(): HIRDefinition.Struct {
        val structName = nominalName()
        return currentModule.findStructDef(structName)
    }

    fun Type.fieldInfo(name: Name, typeArgs: List<Type>?): Pair<Type, Int> =
        getStructDef().getField(name, typeArgs)

    fun HIRExpression.storeRefField(fieldName: Name, value: HIRExpression) {
        val ref = this
        val refType = ref.type
        check(refType is Type.Ref)
        val (fieldType, fieldIndex) = ref.type.fieldInfo(fieldName, refType.inner.typeArgs())
        check(
            typeAnalyzer.isTypeAssignableTo(source = value.type, destination = fieldType)
        )
        emit(
            HIRStatement.StoreRefField(
                currentLocation,
                ref,
                fieldName,
                fieldIndex,
                value
            )
        )
    }

    fun HIRExpression.loadRefField(fieldName: Name, asName: String? = null): HIRExpression {
        val name = namingCtx.makeUniqueName(asName ?: "")
        val ref = this
        val refType = ref.type
        check(refType is Type.Ref)
        val structDef = refType.getStructDef()
        val (fieldType, fieldIndex) = structDef.getField(fieldName, refType.inner.typeArgs())

        emit(
            HIRStatement.LoadRefField(
                currentLocation,
                name,
                ref,
                fieldType,
                fieldName,
                fieldIndex
            )
        )

        return HIRExpression.LocalRef(
            currentLocation,
            fieldType,
            name
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
        val stderrVal = emitCall(libhdcFn("get_stderr").ref(), emptyList()).result()
        when (value.type) {
            is Type.Ptr -> {
                emitCall(
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
        return currentModule.findGlobalFunction(qn("hades", "libhdc", name))
    }

    fun HIROperand.typeApplication(args: List<Type>): HIROperand {
        val ty = type
        check(ty is Type.ForAll)
        check(ty.params.size == args.size)
        val substitution = ty.params.zip(args).toSubstitution()
        return emit(
            HIRStatement.TypeApplication(
                currentLocation,
                namingCtx.makeUniqueName(),
                ty.body.applySubstitution(substitution),
                this,
                args
            )
        ).result()
    }

    fun HIRExpression.fieldPtr(name: String): HIRExpression.LocalRef {
        return fieldPtr(namingCtx.makeName(name))
    }

    fun HIRExpression.ptrCast(toPointerOfType: Type, nameHint: String = ""): HIRExpression.LocalRef {
        val s = emit(
            HIRStatement.PointerCast(
                currentLocation,
                namingCtx.makeUniqueName(nameHint),
                toPointerOfType,
                this
            )
        )
        return HIRExpression.LocalRef(
            currentLocation,
            s.type,
            s.name
        )
    }

    fun HIRExpression.load(name: Name = namingCtx.makeUniqueName()): HIRExpression.LocalRef {
        val ptrTy = type
        check(ptrTy is Type.Ptr)
        emit(HIRStatement.Load(currentLocation, name, this))
        return HIRExpression.LocalRef(currentLocation, ptrTy.to, name)
    }

    fun HIRParam.ref(): HIRExpression.ParamRef {
        return HIRExpression.ParamRef(
            currentLocation,
            type,
            name,
            binder
        )
    }
}

fun <T : HIRStatement> HIRBuilder.emit(statement: T): T {
    requireNotNull(currentStatements).add(statement)
    return statement
}

fun <T : HIRDefinition> HIRBuilder.emitDef(definition: T): T {
    currentModule.addDefinition(definition)
    return definition
}

fun HIRBuilder.emitAll(statements: Iterable<HIRStatement>) {
    requireNotNull(currentStatements).addAll(statements)
}

fun HIRBuilder.emitIntToPtr(expression: HIROperand, to: Type.Ptr): HIROperand {
    val s = emit(
        HIRStatement.IntToPtr(
            expression.location,
            namingCtx.makeUniqueName(),
            to,
            expression
        )
    )
    return HIRExpression.LocalRef(
        s.location,
        s.type,
        s.name
    )
}

fun HIRBuilder.emitPtrToInt(expression: HIROperand, to: Type): HIROperand {
    require(to.isIntegral())
    val s = emit(
        HIRStatement.PtrToInt(
            expression.location,
            namingCtx.makeUniqueName(),
            to,
            expression
        )
    )
    return HIRExpression.LocalRef(
        s.location,
        s.type,
        s.name
    )
}

fun HIRBuilder.emitIntegerConvert(expression: HIROperand, to: Type): HIROperand {
    val s = emit(
        HIRStatement.IntegerConvert(
            expression.location,
            namingCtx.makeUniqueName(),
            to,
            expression
        )
    )
    return HIRExpression.LocalRef(
        s.location,
        s.type,
        s.name
    )
}

fun HIRBuilder.emitTypeApplication(lhs: HIROperand, args: List<Type>): HIRStatement.TypeApplication {
    val lhsType = lhs.type
    check(lhsType is Type.ForAll)
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

fun HIRBuilder.emitAllocRef(namePrefix: String = "", ofType: Type): HIRStatement.AllocRef {
    val name = namingCtx.makeUniqueName(namePrefix)
    return emit(HIRStatement.AllocRef(currentLocation, name, Type.Ref(ofType)))
}

fun HIRBuilder.emitCall(
    callee: HIROperand,
    args: List<HIRExpression>,
    location: SourceLocation = currentLocation,
    name: Name = namingCtx.makeUniqueName()
): HIRStatement.Call {
    val calleeType = callee.type
    check(calleeType is Type.FunctionPtr) {
        "Expected ${calleeType.prettyPrint()} to be a function pointer"
    }
    check(calleeType.from.size == args.size) {
        "Wrong number of args to fn of type ${calleeType.prettyPrint()}"
    }

    for ((expected, arg) in calleeType.from.zip(args)) {
        arg.type.verifyAssignableTo(expected)
    }

    return emit(HIRStatement.Call(location, calleeType.to, name, callee, args))
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

@Suppress("unused")
fun HIRBuilder.trueValue(location: SourceLocation = currentLocation): HIRConstant.BoolValue {
    return HIRConstant.BoolValue(location, Type.Bool, true)
}

@Suppress("unused")
fun HIRBuilder.falseValue(location: SourceLocation = currentLocation): HIRConstant.BoolValue {
    return HIRConstant.BoolValue(location, Type.Bool, false)
}

internal fun HIRBuilder.qn(vararg names: String): QualifiedName {
    return QualifiedName(names.toList().map { namingCtx.makeName(it) })
}

@Suppress("unused")
fun HIRBuilder.emitDumpCStr(operand: HIROperand) {
    val puts = currentModule.findDefinition(qn("_hdc_puts"))
    check(puts is HIRDefinition.ExternFunction)

    check(operand.type == Type.u8.ptr() || operand.type == Type.u8.mutPtr())
    emitCall(
        puts.ref(),
        listOf(operand)
    )
}

fun HIRStatement.AllocRef.ref(): HIRExpression =
    HIRExpression.LocalRef(
        location = location,
        type = type,
        name = name
    )
