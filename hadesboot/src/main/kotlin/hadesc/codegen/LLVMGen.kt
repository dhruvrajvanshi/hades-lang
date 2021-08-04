package hadesc.codegen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.hir.passes.SimplifyControlFlow
import hadesc.ir.BinaryOperator
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.unit
import llvm.*
import org.bytedeco.llvm.LLVM.LLVMModuleRef
import org.bytedeco.llvm.LLVM.LLVMValueRef
import org.bytedeco.llvm.global.LLVM

class LLVMGen(
    private val ctx: Context,
    private val hir: HIRModule
) {
    private val llvmCtx = LLVM.LLVMContextCreate()
    private val llvmModule = LLVM.LLVMModuleCreateWithNameInContext(ctx.options.main.toString(), llvmCtx)
    private val builder = LLVM.LLVMCreateBuilderInContext(llvmCtx)
    private val dataLayout = LLVM.LLVMGetModuleDataLayout(llvmModule.ref)
    private val diBuilder = LLVM.LLVMCreateDIBuilder(llvmModule)
    private var currentFunction: LLVMValueRef? = null
    private var currentHIRFunction: HIRDefinition.Function? = null
    private val localPointers = mutableMapOf<Name, Value>()
    private val params = mutableMapOf<Name, Value>()

    private val log = logger()

    fun lower(): LLVMModuleRef {
        for (definition in hir.definitions) {
            lowerDefinition(definition)
        }
        log.debug("LLVM module: \n" + llvmModule.prettyPrint())
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
            val thisPtr = buildAlloca(instanceType, "this", LLVM.LLVMABIAlignmentOfType(dataLayout, instanceType))
            var index = -1
            for (field in definition.fields) {
                index++
                val fieldPtr = buildStructGEP(thisPtr, index, "field_$index")
                val value = fn.getParameter(index)
                buildStore(toPointer = fieldPtr, value = value)
            }
            val instance = buildLoad(thisPtr, "instance")
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
            check(constructorPtrType is Type.Ptr && constructorPtrType.to is Type.Function)
            val loweredType = lowerType(constructorPtrType.to)
            llvmModule.addFunction(name, loweredType)
        }


    }




    private fun lowerFunction(definition: HIRDefinition.Function) {
        check(currentHIRFunction == null)
        check(currentFunction == null)
        currentHIRFunction = definition
        val fn = getFunctionRef(definition)
        currentFunction = fn
        localPointers.clear()
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
        return llvmModule.getFunction(name) ?: llvmModule.addFunction(name, lowerType(definition.type))
    }
    private fun getFunctionRef(definition: HIRDefinition.Function): LLVMValueRef {
        val name = getFunctionName(definition)
        return llvmModule.getFunction(name) ?:
            llvmModule.addFunction(name, lowerType(definition.type))
    }


    private fun getConstRef(definition: HIRDefinition.ExternConst): Value {
        val name = definition.externName.text
        return llvmModule.getNamedGlobal(name) ?:
            llvmModule.addGlobal(name, lowerType(definition.type))
    }

    private fun getConstRef(definition: HIRDefinition.Const): Value {
        val name = definition.name.mangle()
        return llvmModule.getNamedGlobal(name) ?:
            llvmModule.addGlobal(name, lowerType(definition.type))
    }

    private fun getStructRef(definition: HIRDefinition.Struct): LLVMValueRef {
        return getStructConstructor(definition)
    }

    private fun getFunctionName(definition: HIRDefinition.Function): String {
        val originalName = definition.name.mangle()
        return if (originalName == "main") "hades_main" else originalName
    }


    private fun lowerStatement(statement: HIRStatement): Unit =
        when (statement) {
            is HIRStatement.Assignment -> lowerAssignment(statement)
            is HIRStatement.Expression -> { lowerExpression(statement.expression); unit }
            is HIRStatement.Return -> lowerReturnStatement(statement)
            is HIRStatement.ReturnVoid -> { builder.buildRetVoid(); unit }
            is HIRStatement.Store -> lowerStore(statement)
            is HIRStatement.ValDeclaration -> lowerValDeclaration(statement)
            is HIRStatement.Branch -> lowerBranch(statement)
            is HIRStatement.ConditionalBranch -> lowerConditionalBranch(statement)
            is HIRStatement.If,
            is HIRStatement.While -> requireUnreachable {
                "Unexpected control flow branch should have been desugared by ${SimplifyControlFlow::class.simpleName}"
            }
        }

    private fun lowerStore(statement: HIRStatement.Store) {
        builder.buildStore(
            lowerExpression(statement.ptr),
            lowerExpression(statement.value),
        )
    }

    private fun lowerAssignment(statement: HIRStatement.Assignment) {
        log.debug("${statement.name.text} = ${statement.value.prettyPrint()}")
        val value = lowerExpression(statement.value)
        val pointer = checkNotNull(localPointers[statement.name])
        builder.buildStore(
            value = value,
            toPointer = pointer,
        )
    }

    private fun lowerValDeclaration(statement: HIRStatement.ValDeclaration) {
        val value = builder.buildAlloca(
            lowerType(statement.type),
            statement.name.text,
            LLVM.LLVMABIAlignmentOfType(dataLayout, lowerType(statement.type))
        )
        localPointers[statement.name] = value
    }

    private fun lowerBranch(statement: HIRStatement.Branch) {
        builder.buildBr(getBlock(statement.toBranchName))
    }

    private fun lowerConditionalBranch(statement: HIRStatement.ConditionalBranch) {
        builder.buildCondBr(
            lowerExpression(statement.condition),
            getBlock(statement.trueBranchName),
            getBlock(statement.falseBranchName)
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
        builder.buildRet(lowerExpression(statement.expression))
    }

    private fun lowerExpression(expression: HIRExpression): Value {
        log.debug("${expression.location}: ${expression.prettyPrint()}")
        return when (expression) {
            is HIRExpression.AddressOf -> lowerAddressOf(expression)
            is HIRExpression.ArrayIndex -> lowerArrayIndex(expression)
            is HIRExpression.BinOp -> lowerBinOp(expression)
            is HIRExpression.Call -> lowerCallExpression(expression)
            is HIRExpression.Constant -> lowerConstant(expression.constant)
            is HIRExpression.GetStructField -> lowerGetStructField(expression)
            is HIRExpression.GetStructFieldPointer -> lowerGetStructFieldPointer(expression)
            is HIRExpression.GlobalRef -> lowerGlobalRef(expression)
            is HIRExpression.IntegerConvert -> lowerIntegerConvert(expression)
            is HIRExpression.InvokeClosure -> requireUnreachable()
            is HIRExpression.Load -> lowerLoad(expression)
            is HIRExpression.Not -> lowerNotExpression(expression)
            is HIRExpression.NullPtr -> lowerNullPtr(expression)
            is HIRExpression.ParamRef -> lowerParamRef(expression)
            is HIRExpression.PointerCast -> lowerPointerCast(expression)
            is HIRExpression.SizeOf -> lowerSizeOf(expression)
            is HIRExpression.UnsafeCast -> requireUnreachable()
            is HIRExpression.ValRef -> lowerValRef(expression)
            is HIRExpression.TypeApplication -> requireUnreachable()
            is HIRExpression.TraitMethodRef -> requireUnreachable()
            is HIRExpression.Closure -> requireUnreachable()
            is HIRExpression.When -> requireUnreachable()
            is HIRExpression.BlockExpression -> requireUnreachable()
        }
    }

    private fun lowerArrayIndex(expression: HIRExpression.ArrayIndex): Value {
        return builder.buildExtractElement(
            lowerExpression(expression.array),
            lowerExpression(expression.index),
            ctx.makeUniqueName().text,
        )
    }

    private fun lowerGetStructFieldPointer(expression: HIRExpression.GetStructFieldPointer): Value {
        return builder.buildStructGEP(
            pointer = lowerExpression(expression.lhs),
            name = ctx.makeUniqueName().text,
            index = expression.memberIndex
        )
    }

    private fun lowerSizeOf(expression: HIRExpression.SizeOf): Value {
        return Value(LLVM.LLVMSizeOf(lowerType(expression.ofType).ref))
    }

    private fun lowerNullPtr(expression: HIRExpression.NullPtr): Value {
        return lowerType(expression.type).getConstantNullPointer()
    }

    private fun lowerBinOp(expression: HIRExpression.BinOp): Value {
        val name = ctx.makeUniqueName().text
        return if (isPredicateOperator(expression.operator)) {
            builder.buildICmp(
                lowerPredicateOperator(expression.operator),
                lowerExpression(expression.lhs),
                lowerExpression(expression.rhs),
                name
            )
        } else {
            builder.buildBinOp(
                lowerOperator(expression.operator),
                lowerExpression(expression.lhs),
                lowerExpression(expression.rhs),
                name)
        }
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

    private fun lowerPredicateOperator(operator: BinaryOperator): IntPredicate {
        return when(operator) {
            BinaryOperator.EQUALS -> IntPredicate.EQ
            BinaryOperator.NOT_EQUALS -> IntPredicate.NE
            BinaryOperator.GREATER_THAN -> IntPredicate.SGT
            BinaryOperator.GREATER_THAN_EQUAL -> IntPredicate.SGE
            BinaryOperator.LESS_THAN -> IntPredicate.SLT
            BinaryOperator.LESS_THAN_EQUAL -> IntPredicate.SLE
            else -> requireUnreachable()
        }
    }

    private fun lowerAddressOf(expression: HIRExpression.AddressOf): Value {
        return checkNotNull(localPointers[expression.name])
    }

    private fun lowerPointerCast(expression: HIRExpression.PointerCast): Value {
        return builder.buildPointerCast(
            lowerExpression(expression.value),
            pointerType(lowerType(expression.toPointerOfType)),
            ctx.makeUniqueName().text
        )
    }

    private fun lowerParamRef(expression: HIRExpression.ParamRef): Value {
        return checkNotNull(params[expression.name])
    }

    private fun lowerGetStructField(expression: HIRExpression.GetStructField): Value {
        return builder.buildExtractValue(
            lowerExpression(expression.lhs),
            expression.index,
            ctx.makeUniqueName().text
        )
    }

    private fun lowerLoad(expression: HIRExpression.Load): Value {
        return builder.buildLoad(
            lowerExpression(expression.ptr),
            ctx.makeUniqueName().text
        )
    }

    private fun lowerIntegerConvert(expression: HIRExpression.IntegerConvert): Value {
        val fromType = expression.value.type
        require(fromType is Type.Integral || fromType is Type.Size)
        val toType = expression.type
        require(toType is Type.Integral || toType is Type.Size)

        val fromSize = fromType.sizeInBits

        val toSize = toType.sizeInBits

        return when {
            toSize > fromSize -> {
                builder.buildZExt(
                    lowerExpression(expression.value),
                    toType = lowerType(toType),
                    name = ctx.makeUniqueName().text,
                )
            }
            toSize < fromSize -> {
                builder.buildTrunc(
                    lowerExpression(expression.value),
                    lowerType(toType),
                    name = ctx.makeUniqueName().text
                )
            }
            else -> {
                builder.buildBitCast(
                    lowerExpression(expression.value),
                    lowerType(toType),
                    ctx.makeUniqueName().text
                )
            }
        }
    }

    private fun lowerNotExpression(expression: HIRExpression.Not): Value {
        return builder.buildNot(lowerExpression(expression.expression), ctx.makeUniqueName().text)
    }

    private fun lowerValRef(expression: HIRExpression.ValRef): Value {
        val ptr = checkNotNull(localPointers[expression.name])
        return builder.buildLoad(ptr, ctx.makeUniqueName().text)
    }

    private fun lowerConstant(constant: HIRConstant): Value =
        when (constant) {
            is HIRConstant.ArrayLiteral -> lowerArrayLiteral(constant)
            is HIRConstant.BoolValue -> if (constant.value) trueValue else falseValue
            is HIRConstant.ByteString -> lowerByteString(constant)
            is HIRConstant.FloatValue -> lowerFloatLiteral(constant)
            is HIRConstant.IntValue -> lowerIntLiteral(constant)
        }

    private fun lowerArrayLiteral(constant: HIRConstant.ArrayLiteral): Value {
        return constantArray(
            lowerType(constant.type.ofType),
            constant.items.map { lowerConstant(it) },
            constant.items.size
        )
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
        val constStringRef = constantString(text, nullTerminate = false, context = llvmCtx)
        val globalRef = llvmModule.addGlobal(
            stringLiteralName(),
            constStringRef.getType()
        )
        globalRef.setInitializer(constStringRef)
        return globalRef
    }

    private fun lowerGlobalRef(expression: HIRExpression.GlobalRef): Value {
        return when (val definition = hir.findGlobalDefinition(expression.name)) {
            is HIRDefinition.ExternFunction ->
                getFunctionRef(definition)
            is HIRDefinition.Function -> getFunctionRef(definition)
            is HIRDefinition.Struct -> getStructRef(definition)
            is HIRDefinition.ExternConst -> builder.buildLoad(getConstRef(definition), ctx.makeUniqueName().text)
            is HIRDefinition.Const -> lowerExpression(definition.initializer)
//                builder.buildLoad(getConstRef(definition), ctx.makeUniqueName().text)
            else -> requireUnreachable { definition.javaClass.name }
        }
    }


    private fun lowerCallExpression(expression: HIRExpression.Call): Value {
        val name = if (expression.type is Type.Void) null else ctx.makeUniqueName()
        return builder.buildCall(
            callee = lowerExpression(expression.callee),
            args = expression.args.map { lowerExpression(it) },
            name = name?.text
        )
    }

    private val voidTy = voidType(llvmCtx)
    private val boolTy = intType(1, llvmCtx)
    private val trueValue = constantInt(boolTy, 1, false)
    private val falseValue = constantInt(boolTy, 0, false)
    private val sizeTy = intType(64, llvmCtx) // FIXME: This isn't portable

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
        is Type.Function -> {
            functionType(
                returns = lowerType(type.to),
                types = type.from.map { lowerType(it) },
                variadic = false
            )
        }
        is Type.Constructor ->  {
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
            val maxSizedType = type.members
                .map { lowerType(it) }
                .maxByOrNull { sizeOfType(it) }
            checkNotNull(maxSizedType)
        }
        is Type.Integral -> intType(type.size, llvmCtx)
        is Type.FloatingPoint -> floatType(type.size, llvmCtx)
        is Type.TypeFunction -> requireUnreachable()
        is Type.Uninferrable -> requireUnreachable()
        is Type.AssociatedTypeRef -> requireUnreachable()
        is Type.Select -> requireUnreachable()
        is Type.Array -> arrayType(lowerType(type.ofType), type.length)
    }

    private fun sizeOfType(type: llvm.Type): Long {
        return LLVM.LLVMABISizeOfType(dataLayout, type)
    }

    private val Type.sizeInBits get() = LLVM.LLVMSizeOfTypeInBits(dataLayout, lowerType(this))


    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "_hadesboot_string_literal_$nextLiteralIndex"
    }


}