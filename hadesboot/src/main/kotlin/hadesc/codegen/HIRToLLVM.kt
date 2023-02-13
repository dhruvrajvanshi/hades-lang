package hadesc.codegen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.clampToPowerOfTwo
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.hir.*
import hadesc.hir.BinaryOperator
import hadesc.hir.passes.SimplifyControlFlow
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.unit
import libhades.collections.Stack
import llvm.*
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.javacpp.LongPointer
import org.bytedeco.javacpp.PointerPointer
import org.bytedeco.llvm.LLVM.LLVMMetadataRef
import org.bytedeco.llvm.LLVM.LLVMModuleRef
import org.bytedeco.llvm.LLVM.LLVMValueRef
import org.bytedeco.llvm.global.LLVM

class HIRToLLVM(
    private val ctx: Context,
    private val hir: HIRModule
) {
    private val llvmCtx = LLVM.LLVMContextCreate()
    private val llvmModule = LLVM.LLVMModuleCreateWithNameInContext(ctx.target.output.toString(), llvmCtx)
    private val builder = LLVM.LLVMCreateBuilderInContext(llvmCtx)
    private val dataLayout = LLVM.LLVMGetModuleDataLayout(llvmModule.ref)
    private val diBuilder = LLVM.LLVMCreateDIBuilder(llvmModule)
    private var currentFunction: LLVMValueRef? = null
    private var currentHIRFunction: HIRDefinition.Function? = null
    private val localValues = mutableMapOf<Name, Value>()
    private val params = mutableMapOf<Name, Value>()

    private val log = logger(HIRToLLVM::class.java)
    private val i32Ty = intType(32, llvmCtx)

    @Suppress("unused")
    private val metadataTy = LLVM.LLVMMetadataTypeInContext(llvmCtx)
    private val errorStack = Stack<HIRStatement>()

    fun lower(): LLVMModuleRef {
        if (shouldEmitDebugSymbols) {
            llvmModule.addModuleFlag("Debug Info Version", constantInt(i32Ty, 3).asMetadata())
            llvmModule.addModuleFlag("Dwarf Version", constantInt(i32Ty, 4).asMetadata())
        }

        for (definition in hir.definitions) {
            lowerDefinition(definition)
        }
        if (ctx.options.dumpLLVMModule) {
            print(llvmModule.prettyPrint())
        }

        LLVM.LLVMDIBuilderFinalize(diBuilder)
        if (ctx.options.enableLLVMVerifier) {
            LLVM.LLVMVerifyModule(llvmModule, LLVM.LLVMAbortProcessAction, null as PointerPointer<*>?)
        }
        return llvmModule
    }

    private fun lowerDefinition(definition: HIRDefinition): Unit =
        when (definition) {
            is HIRDefinition.Const -> lowerConstDefinition(definition)
            is HIRDefinition.ExternConst -> unit
            is HIRDefinition.ExternFunction -> unit
            is HIRDefinition.Function -> lowerFunction(definition)
            is HIRDefinition.Implementation -> requireUnreachable()
            is HIRDefinition.Struct -> lowerStructDef(definition)
        }

    private fun lowerConstDefinition(definition: HIRDefinition.Const) {
        val global = getConstRef(definition)
        if (global.getInitializer() == null) {
            global.setInitializer(lowerExpression(definition.initializer))
        }
    }

    private fun lowerStructDef(definition: HIRDefinition.Struct) {
        val fn = getStructConstructor(definition)
        withinBlock(fn.createBlock("entry")) {
            val instanceType = lowerType(definition.instanceType())
            val thisPtr = buildAlloca(instanceType, "this")
            var index = -1
            for (field in definition.fields) {
                index++
                if (field.second is Type.Void) {
                    continue
                }
                val fieldPtr = buildStructGEP(
                    instanceType,
                    thisPtr,
                    index,
                    "field_$index"
                )
                val value = fn.getParameter(index)
                buildStore(toPointer = fieldPtr, value = value)
            }
            val instance = buildLoad(instanceType, thisPtr, "instance")
            buildRet(instance)
        }
    }

    private fun withinBlock(basicBlock: BasicBlock, fn: Builder.() -> Unit) {
        builder.positionAtEnd(basicBlock)
        builder.fn()
    }

    private fun getStructConstructor(def: HIRDefinition.Struct): FunctionValue {
        val name = def.name.mangle()
        val existing = llvmModule.getFunction(name)?.asFunctionValue()
        return if (existing != null) {
            existing
        } else {
            val constructorPtrType = def.constructorType
            check(constructorPtrType is Type.FunctionPtr)
            llvmModule.addFunction(
                name,
                from = constructorPtrType.from.map { lowerType(it) },
                to = lowerType(constructorPtrType.to)
            )
        }
    }

    private val shouldEmitDebugSymbols get() = ctx.options.debugSymbols

    private val fileScopeCache = mutableMapOf<SourcePath, LLVMMetadataRef>()
    private fun getFileScope(file: SourcePath): LLVMMetadataRef = fileScopeCache.getOrPut(file) {
        val f = diBuilder.createFile(file.path.fileName.toString(), file.path.toAbsolutePath().parent.toString())
        diBuilder.createCompileUnit(f)
        f
    }

    private var currentFunctionMetadata: LLVMMetadataRef? = null

    private fun emitDebugSymbols(value: LLVMValueRef, statement: HIRStatement.Alloca) {
        currentFunction
        val meta = LLVM.LLVMDIBuilderCreateAutoVariable(
            diBuilder,
            currentFunctionMetadata,
            statement.name.text,
            statement.name.text.length.toLong(),
            getFileScope(statement.location.file),
            statement.location.start.line,
            statement.type.debugInfo,
            true.toLLVMBool(),
            LLVM.LLVMDIFlagZero,
            value.getType().alignment().bits
        )

        val callee = llvmModule.getIntrinsicDeclaration(
            "llvm.dbg.addr",
            listOf()
        )
        builder.buildCall(
            functionType(
                types = listOf(
                    metadataTy,
                    metadataTy,
                    metadataTy
                ),
                returns = voidTy,
                variadic = false
            ),
            callee,
            listOf(
                value.asMetadata().asValue(),
                meta.asValue(),
                LLVM.LLVMDIBuilderCreateExpression(
                    diBuilder,
                    null as LongPointer?,
                    0
                ).asValue()
            ),
            name = null
        )
    }

    private fun attachDebugInfo(definition: HIRFunctionSignature, fn: FunctionValue) {
        if (!shouldEmitDebugSymbols) {
            return
        }

        val fileScope = getFileScope(definition.location.file)
        val defType = definition.type
        check(defType is Type.FunctionPtr)
        val typeDI = LLVM.LLVMDIBuilderCreateSubroutineType(
            diBuilder,
            fileScope,
            defType.from.map { it.debugInfo }.asPointerPointer(),
            defType.from.size,
            LLVM.LLVMDIFlagZero
        )

        val name = definition.name.names.joinToString(".") { it.text }
        val meta = diBuilder.createFunction(
            scope = fileScope,
            name = name,
            linkageName = name,
            file = fileScope,
            lineno = definition.location.start.line,
            ty = typeDI,
            isLocalToUnit = true,
            isDefinition = true,
            scopeLine = definition.location.start.line,
            flags = LLVM.LLVMDIFlagZero

        )
        currentFunctionMetadata = meta
        LLVM.LLVMGlobalSetMetadata(fn, LLVM.LLVMMDStringMetadataKind, meta)
        LLVM.LLVMSetCurrentDebugLocation2(builder, null)
    }

    private val Type.debugInfo get(): LLVMMetadataRef = when (this) {
        is Type.Error -> requireUnreachable()
        Type.Void -> diBuilder.createBasicType("Void", 0)
        Type.Bool -> diBuilder.createBasicType("Bool", sizeInBits)
        is Type.Integral -> diBuilder.createBasicType(
            (if (isSigned) "s" else "u") + sizeInBits,
            sizeInBits
        )
        is Type.FloatingPoint -> diBuilder.createBasicType("f$sizeInBits", sizeInBits)
        is Type.Size -> diBuilder.createBasicType(if (isSigned) "usize" else "isize", sizeInBits)
        is Type.Ptr -> LLVM.LLVMDIBuilderCreatePointerType(
            diBuilder,
            to.debugInfo,
            sizeInBits,
            8,
            0,
            null as BytePointer?,
            0
        )
        is Type.FunctionPtr -> LLVM.LLVMDIBuilderCreateNullPtrType(diBuilder)
        is Type.Constructor -> diBuilder.createBasicType(name.mangle(), sizeInBits)
        is Type.UntaggedUnion -> LLVM.LLVMDIBuilderCreateNullPtrType(diBuilder)
        is Type.ParamRef,
        is Type.TypeFunction,
        is Type.GenericInstance,
        is Type.Application,
        is Type.AssociatedTypeRef,
        is Type.Closure,
        is Type.Select -> requireUnreachable()
    }

    private fun lowerFunction(definition: HIRDefinition.Function) {
        check(currentHIRFunction == null)
        check(currentFunction == null)
        currentHIRFunction = definition
        val fn = getFunctionRef(definition)
        if (shouldEmitDebugSymbols) {
            attachDebugInfo(definition.signature, fn)
        }
        currentFunction = fn
        localValues.clear()
        blocks.clear()
        params.clear()
        definition.params.forEachIndexed { index, param ->
            params[param.name] = fn.getParameter(index)
        }
        for (basicBlock in definition.basicBlocks) {
            lowerBlock(basicBlock)
        }

        currentHIRFunction = null
        currentFunction = null
    }

    private fun getFunctionRef(definition: HIRDefinition.ExternFunction): LLVMValueRef {
        val name = definition.externName.text
        return llvmModule.getFunction(name) ?: llvmModule.addFunction(
            name,
            from = definition.type.from.map { lowerType(it) },
            to = lowerType(definition.type.to)
        )
    }
    private fun getFunctionRef(definition: HIRDefinition.Function): LLVMValueRef {
        val name = getFunctionName(definition)
        val fnType = definition.type
        check(fnType is Type.FunctionPtr)
        return llvmModule.getFunction(name)
            ?: llvmModule.addFunction(
                name,
                from = fnType.from.map { lowerType(it) },
                to = lowerType(fnType.to)
            )
    }

    private fun getConstRef(definition: HIRDefinition.ExternConst): Value {
        val name = definition.externName.text
        return llvmModule.getNamedGlobal(name)
            ?: llvmModule.addGlobal(name, lowerType(definition.type))
    }

    private fun getConstRef(definition: HIRDefinition.Const): Value {
        val name = definition.name.mangle()
        return llvmModule.getNamedGlobal(name)
            ?: llvmModule.addGlobal(name, lowerType(definition.type))
    }

    private fun getStructRef(definition: HIRDefinition.Struct): LLVMValueRef {
        return getStructConstructor(definition)
    }

    private fun getFunctionName(definition: HIRDefinition.Function): String {
        val originalName = definition.name.mangle()
        return if (originalName == "main") "hades_main" else originalName
    }

    private fun lowerStatement(statement: HIRStatement) {
        errorStack.push(statement)
        if (shouldEmitDebugSymbols) {
            val location = LLVM.LLVMDIBuilderCreateDebugLocation(
                llvmCtx,
                statement.location.start.line,
                statement.location.start.column,
                currentFunctionMetadata,
                null
            )
            LLVM.LLVMSetCurrentDebugLocation2(builder, location)
        }
        exhaustive(
            when (statement) {
                is HIRStatement.NameBinder -> lowerNameBinder(statement)
                is HIRStatement.Return -> lowerReturnStatement(statement)
                is HIRStatement.Store -> lowerStore(statement)
                is HIRStatement.SwitchInt -> lowerSwitchInt(statement)
                is HIRStatement.Move -> {
                    // Move is  a no-op
                }
                is HIRStatement.MatchInt,
                is HIRStatement.While -> requireUnreachable {
                    "Unexpected control flow branch should have been desugared by ${SimplifyControlFlow::class.simpleName}"
                }
                is HIRStatement.Jump -> TODO()
                is HIRStatement.Memcpy -> lowerMemcpy(statement)
            }
        )
        errorStack.pop()
    }

    private fun lowerMemcpy(statement: HIRStatement.Memcpy) {
        LLVM.LLVMBuildMemCpy(
            builder,
            lowerExpression(statement.destination),
            statement.destinationType.alignment().bytes,
            lowerExpression(statement.source),
            statement.sourceType.alignment().bytes,
            lowerExpression(statement.bytes)
        )
    }

    private fun lowerNameBinder(statement: HIRStatement.NameBinder) {
        val value = when (statement) {
            is HIRStatement.Alloca -> {
                lowerAlloca(statement)
            }
            is HIRStatement.Load -> lowerLoadStatement(statement)
            is HIRStatement.Call -> lowerCallStatement(statement)
            is HIRStatement.GetStructField -> lowerGetStructField(statement)
            is HIRStatement.GetStructFieldPointer -> lowerGetStructFieldPointer(statement)
            is HIRStatement.Not -> lowerNotStatement(statement)
            is HIRStatement.IntegerConvert -> lowerIntegerConvert(statement)
            is HIRStatement.PointerCast -> lowerPointerCast(statement)
            is HIRStatement.TypeApplication -> requireUnreachable()
            is HIRStatement.BinOp -> lowerBinOp(statement)
            is HIRStatement.IntToPtr -> lowerIntToPtr(statement)
            is HIRStatement.PtrToInt -> lowerPtrToInt(statement)
            is HIRStatement.AllocateClosure -> requireUnreachable()
            is HIRStatement.InvokeClosure -> requireUnreachable()
        }

        if (value != null) {
            localValues[statement.name] = value
        }
    }

    private fun lowerPtrToInt(statement: HIRStatement.PtrToInt): Value {
        return LLVM.LLVMBuildPtrToInt(
            builder,
            lowerExpression(statement.expression),
            lowerType(statement.type),
            statement.name.text
        )
    }

    private fun lowerIntToPtr(statement: HIRStatement.IntToPtr): Value {
        return LLVM.LLVMBuildIntToPtr(
            builder,
            lowerExpression(statement.expression),
            lowerType(statement.type),
            statement.name.text
        )
    }

    private fun lowerLoadStatement(statement: HIRStatement.Load): Value? {
        val ptrTy = statement.ptr.type
        check(ptrTy is Type.Ptr)
        if (ptrTy.to is Type.Void) {
            // loads from void pointers are no-ops because they don't have a size
            // since ptr is an operand, it can't have any side effect, so lowering
            // it is not required.
            return null
        }
        return builder.buildLoad(
            name = statement.name.text,
            ptr = lowerOperand(statement.ptr),
            type = lowerType(ptrTy.to)
        )
    }

    private fun lowerStore(statement: HIRStatement.Store) {
        if (statement.value.type is Type.Void) {
            return
        }
        val rhs = lowerExpression(statement.value)
        if (statement.value.type != Type.Void) {
            val ptr = lowerOperand(statement.ptr)
            builder.buildStore(ptr, rhs)
        }
    }

    private fun lowerAlloca(statement: HIRStatement.Alloca): Value? {
        if (statement.type is Type.Void) {
            return null
        }
        val loweredType = lowerType(statement.type)
        val value = builder.buildAlloca(
            loweredType,
            statement.name.text
        )

        if (ctx.options.debugSymbols) {
            emitDebugSymbols(value, statement)
        }
        return value
    }

    private fun lowerSwitchInt(statement: HIRStatement.SwitchInt) {
        builder.buildSwitch(
            lowerExpression(statement.condition),
            statement.cases.map {
                lowerConstant(it.value) to getBlock(it.block)
            },
            getBlock(statement.otherwise)
        )
    }
    private val blocks = mutableMapOf<Pair<String, Name>, BasicBlock>()
    private fun getBlock(blockName: Name): BasicBlock {
        val fnName = checkNotNull(currentFunction?.getName())
        return blocks.computeIfAbsent(fnName to blockName) {
            checkNotNull(currentFunction).createBlock(it.second.text)
        }
    }

    private fun lowerBlock(block: HIRBlock) {
        if (block.statements.isEmpty()) return
        val basicBlock = getBlock(block.name)
        builder.positionAtEnd(basicBlock)
        for (statement in block.statements) {
            lowerStatement(statement)
        }
    }

    private fun lowerReturnStatement(statement: HIRStatement.Return) {
        if (statement.expression.type == Type.Void) {
            builder.buildRetVoid()
        } else {
            builder.buildRet(lowerExpression(statement.expression))
        }
    }

    private fun lowerExpression(expression: HIRExpression): Value {
        log.debug("${expression.location}: ${expression.prettyPrint()}")

        if (shouldEmitDebugSymbols) {
            val location = LLVM.LLVMDIBuilderCreateDebugLocation(
                llvmCtx,
                expression.location.start.line,
                expression.location.start.column,
                currentFunctionMetadata,
                null
            )
            LLVM.LLVMSetCurrentDebugLocation2(builder, location)
        }

        return lowerOperand(expression)
    }

    private fun lowerOperand(expression: HIROperand): Value = when (expression) {
        is HIRExpression.ParamRef -> lowerParamRef(expression)
        is HIRExpression.TraitMethodRef -> requireUnreachable()
        is HIRExpression.GlobalRef -> lowerGlobalRef(expression)
        is HIRConstant -> lowerConstant(expression)
        is HIRExpression.LocalRef -> lowerLocalRef(expression)
    }

    private fun lowerGetStructFieldPointer(expression: HIRStatement.GetStructFieldPointer): Value {
        val ptrTy = expression.lhs.type
        check(ptrTy is Type.Ptr)
        return builder.buildStructGEP(
            structType = lowerType(ptrTy.to),
            pointer = lowerExpression(expression.lhs),
            name = ctx.makeUniqueName().text,
            index = expression.memberIndex
        )
    }

    private fun lowerSizeOf(constant: HIRConstant.SizeOf): Value {
        return Value(LLVM.LLVMSizeOf(lowerType(constant.ofType).ref))
    }

    private fun lowerNullPtr(expression: HIRConstant.NullPtr): Value {
        return lowerType(expression.type).getConstantNullPointer()
    }

    private fun lowerBinOp(statement: HIRStatement.BinOp): Value {
        val name = statement.name.text
        return if (isPredicateOperator(statement.operator)) {
            val isSigned =
                when (statement.type) {
                    is Type.Integral -> statement.type.isSigned
                    is Type.Size -> statement.type.isSigned
                    is Type.FloatingPoint -> true
                    is Type.Bool -> false
                    else -> requireUnreachable { statement.type.prettyPrint() }
                }
            if (statement.lhs.type is Type.FloatingPoint) {
                check(statement.rhs.type is Type.FloatingPoint)
                builder.buildFCmp(
                    lowerFloatPredicateOperator(statement.operator),
                    lowerExpression(statement.lhs),
                    lowerExpression(statement.rhs),
                    name
                )
            } else {
                builder.buildICmp(
                    lowerPredicateOperator(isSigned, statement.operator),
                    lowerExpression(statement.lhs),
                    lowerExpression(statement.rhs),
                    name
                )
            }
        } else {
            if (statement.type is Type.FloatingPoint) {
                builder.buildBinOp(
                    lowerFloatingPointOperator(statement.operator),
                    lowerExpression(statement.lhs),
                    lowerExpression(statement.rhs),
                    name
                )
            } else {
                builder.buildBinOp(
                    lowerOperator(statement.operator),
                    lowerExpression(statement.lhs),
                    lowerExpression(statement.rhs),
                    name
                )
            }
        }
    }

    private fun lowerFloatingPointOperator(operator: BinaryOperator): Opcode = when (operator) {
        BinaryOperator.PLUS -> Opcode.FAdd
        BinaryOperator.MINUS -> Opcode.FSub
        BinaryOperator.TIMES -> Opcode.FMul
        BinaryOperator.DIV -> Opcode.FDiv
        BinaryOperator.REM -> Opcode.FRem
        else -> requireUnreachable()
    }

    private fun lowerOperator(op: BinaryOperator): Opcode {
        return when (op) {
            BinaryOperator.PLUS -> Opcode.Add
            BinaryOperator.MINUS -> Opcode.Sub
            BinaryOperator.TIMES -> Opcode.Mul
            BinaryOperator.AND -> Opcode.And
            BinaryOperator.OR -> Opcode.Or
            else -> requireUnreachable()
        }
    }

    private fun isPredicateOperator(operator: BinaryOperator): Boolean {
        return when (operator) {
            BinaryOperator.EQUALS,
            BinaryOperator.NOT_EQUALS,
            BinaryOperator.GREATER_THAN,
            BinaryOperator.GREATER_THAN_EQUAL,
            BinaryOperator.LESS_THAN,
            BinaryOperator.LESS_THAN_EQUAL -> true
            else -> false
        }
    }

    private fun lowerPredicateOperator(isSigned: Boolean, operator: BinaryOperator): IntPredicate {
        return when (operator) {
            BinaryOperator.EQUALS -> IntPredicate.EQ
            BinaryOperator.NOT_EQUALS -> IntPredicate.NE
            BinaryOperator.GREATER_THAN -> if (isSigned) IntPredicate.SGT else IntPredicate.UGT
            BinaryOperator.GREATER_THAN_EQUAL -> if (isSigned) IntPredicate.SGE else IntPredicate.UGE
            BinaryOperator.LESS_THAN -> if (isSigned) IntPredicate.SLT else IntPredicate.ULT
            BinaryOperator.LESS_THAN_EQUAL -> if (isSigned) IntPredicate.SLE else IntPredicate.ULE
            else -> requireUnreachable()
        }
    }

    private fun lowerFloatPredicateOperator(operator: BinaryOperator): RealPredicate = when (operator) {
        BinaryOperator.EQUALS -> RealPredicate.EQ
        BinaryOperator.NOT_EQUALS -> RealPredicate.NE
        BinaryOperator.GREATER_THAN -> RealPredicate.UGT
        BinaryOperator.GREATER_THAN_EQUAL -> RealPredicate.UGE
        BinaryOperator.LESS_THAN -> RealPredicate.ULT
        BinaryOperator.LESS_THAN_EQUAL -> RealPredicate.ULE
        else -> requireUnreachable()
    }

    private fun lowerPointerCast(statement: HIRStatement.PointerCast): Value {
        return builder.buildPointerCast(
            lowerExpression(statement.value),
            pointerType(lowerType(statement.toPointerOfType)),
            ctx.makeUniqueName().text
        )
    }

    private fun lowerParamRef(expression: HIRExpression.ParamRef): Value {
        return checkNotNull(params[expression.name])
    }

    private fun lowerGetStructField(expression: HIRStatement.GetStructField): Value {
        return builder.buildExtractValue(
            lowerExpression(expression.lhs),
            expression.index,
            ctx.makeUniqueName().text
        )
    }

    private fun lowerIntegerConvert(statement: HIRStatement.IntegerConvert): Value {
        val fromType = statement.value.type
        require(fromType is Type.Integral || fromType is Type.Size || fromType is Type.Ptr)
        val toType = statement.type
        require(toType is Type.Integral || toType is Type.Size || toType is Type.Ptr)

        val fromSize = fromType.sizeInBits

        val toSize = toType.sizeInBits

        return when {
            toSize > fromSize -> {
                builder.buildZExt(
                    lowerExpression(statement.value),
                    toType = lowerType(toType),
                    name = statement.name.text
                )
            }
            toSize < fromSize -> {
                builder.buildTrunc(
                    lowerExpression(statement.value),
                    lowerType(toType),
                    name = statement.name.text
                )
            }
            else -> {
                builder.buildBitCast(
                    lowerExpression(statement.value),
                    lowerType(toType),
                    name = statement.name.text
                )
            }
        }
    }

    private fun lowerNotStatement(statement: HIRStatement.Not): Value {
        return builder.buildNot(lowerExpression(statement.expression), statement.name.text)
    }

    private fun lowerLocalRef(expression: HIRExpression.LocalRef): Value {
        return checkNotNull(localValues[expression.name]) { buildErrorMessage("Local ${expression.name} not defined") }
    }

    private fun buildErrorMessage(message: String): String {
        var resultMessage = "Bug in code generation: $message"
        val currentStatement = errorStack.peek()
        if (currentStatement != null) {
            resultMessage += "\nWhile lowering statement (originally defined " +
                "at ${currentStatement.location.file}:${currentStatement.location.start.line})\n" +
                currentStatement.prettyPrint()

            for (statement in errorStack.items()) {
                resultMessage += statement.prettyPrint()
            }
        }
        return resultMessage
    }

    private fun lowerConstant(constant: HIRConstant): Value =
        when (constant) {
            is HIRConstant.BoolValue -> if (constant.value) trueValue else falseValue
            is HIRConstant.ByteString -> lowerByteString(constant)
            is HIRConstant.FloatValue -> lowerFloatLiteral(constant)
            is HIRConstant.IntValue -> lowerIntLiteral(constant)
            is HIRConstant.NullPtr -> lowerNullPtr(constant)
            is HIRConstant.SizeOf -> lowerSizeOf(constant)
            is HIRConstant.Void -> requireUnreachable()
            is HIRConstant.AlignOf -> lowerAlignOf(constant)
        }

    private fun lowerAlignOf(constant: HIRConstant.AlignOf): Value {
        return Value(LLVM.LLVMAlignOf(lowerType(constant.type)))
    }

    private fun lowerFloatLiteral(constant: HIRConstant.FloatValue): Value {
        return constantFloat(
            lowerType(constant.type),
            constant.value
        )
    }

    private fun lowerIntLiteral(constant: HIRConstant.IntValue): Value {
        return constantInt(
            value = constant.value.toLong(),
            type = lowerType(constant.type),
            signExtend = false
        )
    }

    private fun lowerByteString(constant: HIRConstant.ByteString): Value {
        val text = constant.bytes.decodeToString()
        val constStringRef = constantString(text, dontNullTerminate = false, context = llvmCtx)
        val globalRef = llvmModule.addGlobal(
            stringLiteralName(),
            constStringRef.getType()
        )
        globalRef.setInitializer(constStringRef)
        return builder.buildPointerCast(globalRef, pointerType(byteTy), ctx.makeUniqueName().text)
    }

    private fun lowerGlobalRef(expression: HIRExpression.GlobalRef): Value {
        return when (val definition = hir.findGlobalDefinition(expression.name)) {
            is HIRDefinition.ExternFunction ->
                getFunctionRef(definition)
            is HIRDefinition.Function -> getFunctionRef(definition)
            is HIRDefinition.Struct -> getStructRef(definition)
            is HIRDefinition.ExternConst -> builder.buildLoad(
                lowerType(expression.type),
                getConstRef(definition),
                ctx.makeUniqueName().text
            )
            is HIRDefinition.Const -> lowerExpression(definition.initializer)
//                builder.buildLoad(getConstRef(definition), ctx.makeUniqueName().text)
            else -> requireUnreachable { definition.javaClass.name }
        }
    }

    private fun lowerCallStatement(statement: HIRStatement.Call): Value {
        val calleeType = statement.callee.type
        check(calleeType is Type.FunctionPtr) {
            statement.location
        }
        val name = if (statement.resultType is Type.Void) null else ctx.makeUniqueName()

        val loweredCallee = lowerExpression(statement.callee)
        val args = statement.args.map { lowerExpression(it) }
        return builder.buildCall(
            functionType = functionType(
                returns = lowerType(statement.resultType),
                types = statement.args.map { lowerType(it.type) },
                variadic = false
            ),
            callee = loweredCallee,
            args = args,
            name = name?.text
        )
    }

    private val voidTy = voidType(llvmCtx)
    private val boolTy = intType(1, llvmCtx)
    private val trueValue = constantInt(boolTy, 1, false)
    private val falseValue = constantInt(boolTy, 0, false)
    private val sizeTy = intType(64, llvmCtx) // FIXME: This isn't portable
    private val byteTy = intType(8, llvmCtx)

    private fun ptrTy(to: llvm.Type): llvm.Type {
        return pointerType(to)
    }

    private val structTypes = mutableMapOf<QualifiedName, llvm.Type>()
    private fun lowerType(type: Type): llvm.Type = when (type) {
        is Type.Error -> requireUnreachable {
            "${type.location}"
        }
        Type.Void -> voidTy
        is Type.Bool -> boolTy
        is Type.Ptr -> ptrTy(lowerType(type.to))
        is Type.FunctionPtr -> {
            ptrTy(
                functionType(
                    returns = lowerType(type.to),
                    types = type.from.map { lowerType(it) },
                    variadic = false
                )
            )
        }
        is Type.Constructor -> {
            if (type.name !in structTypes) {
                val binding = hir.findGlobalDefinition(type.name)
                check(binding is HIRDefinition.Struct)
                val memberTypes = binding.fields
                val name = type.name.mangle()
                val structTy = structType(name, llvmCtx)
                structTypes[type.name] = structTy
                structTy.setBody(memberTypes.map { lowerType(it.second) }, packed = false)
                structTy
            } else {
                checkNotNull(structTypes[type.name])
            }
        }
        is Type.ParamRef -> requireUnreachable()
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> requireUnreachable()
        is Type.Size -> sizeTy
        is Type.UntaggedUnion -> {
            val loweredMembers = type.members
                .map { lowerType(it) }
            val maxSize = sizeOfType(checkNotNull(loweredMembers.maxByOrNull { sizeOfType(it) }))
            arrayType(
                intType(8),
                clampToPowerOfTwo(maxSize.toInt())
            )
        }
        is Type.Integral -> intType(type.size, llvmCtx)
        is Type.FloatingPoint -> floatType(type.size, llvmCtx)
        is Type.TypeFunction,
        is Type.AssociatedTypeRef,
        is Type.Closure,
        is Type.Select -> requireUnreachable()
    }

    private fun sizeOfType(type: llvm.Type): Long {
        return LLVM.LLVMABISizeOfType(dataLayout, type)
    }

    private val Type.sizeInBits get() = LLVM.LLVMSizeOfTypeInBits(dataLayout, lowerType(this))

    @Suppress("unused")
    private val Type.sizeInBytes get() = LLVM.LLVMABISizeOfType(dataLayout, lowerType(this))

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "_hadesboot_string_literal_$nextLiteralIndex"
    }

    private fun Type.alignment() = lowerType(this).alignment()
    private fun llvm.Type.alignment() =
        Alignment(LLVM.LLVMABIAlignmentOfType(dataLayout, this))

    private fun Metadata.asValue(): Value =
        this.asValue(llvmCtx)
}

@JvmInline
value class Alignment(val bytes: Int) {
    val bits inline get() = bytes * 8
}
