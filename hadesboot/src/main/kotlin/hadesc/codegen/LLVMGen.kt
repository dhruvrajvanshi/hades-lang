package hadesc.codegen

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.ir.*
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.profile
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import llvm.*
import org.apache.commons.lang3.SystemUtils
import org.bytedeco.javacpp.BytePointer
import org.bytedeco.llvm.LLVM.LLVMMetadataRef
import org.bytedeco.llvm.LLVM.LLVMTargetMachineRef
import org.bytedeco.llvm.LLVM.LLVMValueRef
import org.bytedeco.llvm.global.LLVM

class LLVMGen(private val ctx: Context, private val irModule: IRModule) : AutoCloseable {
    private var currentFunction: LLVMValueRef? = null
    private val log = logger()
    private val llvmCtx = LLVM.LLVMContextCreate()
    private val llvmModule = LLVM.LLVMModuleCreateWithNameInContext(ctx.options.main.toString(), llvmCtx)
    private val builder = LLVM.LLVMCreateBuilderInContext(llvmCtx)
    private val dataLayout = LLVM.LLVMGetModuleDataLayout(llvmModule.ref)
    private val diBuilder = LLVM.LLVMCreateDIBuilder(llvmModule)

    fun generate() = profile("LLVM::generate") {
        log.debug(irModule.prettyPrint())
        llvmModule.addModuleFlag("Debug Info Version", ConstantInt(i32Ty, 3).asMetadata())
        llvmModule.addModuleFlag("Dwarf Version", ConstantInt(i32Ty, 4).asMetadata())

        lower()
        LLVM.LLVMDIBuilderFinalize(diBuilder)
        verifyModule()
        writeModuleToFile()
        if (!ctx.options.lib) {
            linkWithRuntime()
        }
    }

    private fun lower() = profile("LLVM::lower") {
        var defIndex = -1
        for (it in irModule) {
            defIndex++
//            logger().debug("Lowering definition $defIndex of ${irModule.size}")
            lowerDefinition(it)
        }
    }

    private fun sizeOfType(type: llvm.Type): Long {
        return LLVM.LLVMABISizeOfType(dataLayout, type)
    }

    private val Type.sizeInBits get() = LLVM.LLVMSizeOfTypeInBits(dataLayout, lowerType(this))

    private fun lowerDefinition(definition: IRDefinition) {
//        logger().debug("LLVMGen::lowerDefinition")
        return when (definition) {
            is IRFunctionDef -> lowerFunctionDef(definition)
            is IRStructDef -> lowerStructDef(definition)
            is IRExternFunctionDef -> lowerExternFunctionDef(definition)
            is IRConstDef -> lowerConstDef(definition)
            is IRInterfaceDef -> TODO()
            is IRImplementationDef -> TODO()
            is IRExternConstDef -> {}
        }
    }

    private val loweredGlobals = mutableMapOf<IRGlobalName, Value>()
    private fun lowerConstDef(definition: IRConstDef) {
//        logger().debug("LLVMGen::lowerConstDef(${definition.name.prettyPrint()})")
        val name = lowerName(definition.name)
        if (loweredGlobals[definition.name] == null) {
            val global = llvmModule.addGlobal(name, lowerType(definition.type))
            global.setInitializer(lowerExpression(definition.initializer))
            loweredGlobals[definition.name] = requireNotNull(global.getInitializer())
        }
    }

    private fun getConstDefValue(def: IRConstDef): Value  {
        if (loweredGlobals[def.name] == null) {
            lowerConstDef(def)
        }
        return requireNotNull(loweredGlobals[def.name])
    }

    private fun lowerStructDef(definition: IRStructDef) {
//        logger().debug("LLVMGen::lowerStructDef(${definition.globalName.prettyPrint()})")
        val fn = getStructConstructor(definition)
        withinBlock(fn.createBlock("entry")) {
            val instanceType = lowerType(definition.instanceType)
            val thisPtr = buildAlloca(instanceType, "this", LLVM.LLVMABIAlignmentOfType(dataLayout, instanceType))
            var index = -1
            for (field in definition.fields) {
                index++
                val fieldPtr = buildStructGEP(thisPtr, index, "field_$index")
                val value = fn.getParameter(index)
                buildStore(toPointer = fieldPtr, value = value, LLVM.LLVMABIAlignmentOfType(dataLayout, lowerType(field.value)))
            }
            val instance = buildLoad(thisPtr, "instance")
            buildRet(instance)
        }

    }

    private fun lowerExternFunctionDef(definition: IRExternFunctionDef) {
        getDeclaration(definition)
    }

    private fun getExternConstRef(definition: IRExternConstDef): Value {
        return builder.buildLoad(llvmModule.getNamedGlobal(definition.externName.text)
            ?: llvmModule.addGlobal(definition.externName.text, lowerType(definition.type)), ctx.makeUniqueName().text)
    }

    private val Type.debugInfo get(): LLVMMetadataRef = when (this) {
        is Type.Error -> requireUnreachable { "${this.location}" }
        Type.Byte -> diBuilder.createBasicType("Byte", 8)
        Type.Void -> diBuilder.createBasicType("Void", 0)
        Type.Bool -> diBuilder.createBasicType("Bool", sizeInBits)
        is Type.Integral -> diBuilder.createBasicType(
            (if (isSigned) "s" else "u") + sizeInBits,
            sizeInBits
        )
        is Type.FloatingPoint -> diBuilder.createBasicType("f$sizeInBits", sizeInBits)
        Type.Double -> diBuilder.createBasicType("Double", sizeInBits)
        Type.Size -> diBuilder.createBasicType("Size", sizeInBits)
        is Type.Ptr -> LLVM.LLVMDIBuilderCreatePointerType(
            diBuilder,
            to.debugInfo,
            sizeInBits,
            8,
            0,
            null as BytePointer?,
            0
        )
        is Type.Function -> LLVM.LLVMDIBuilderCreateNullPtrType(diBuilder)
        is Type.Constructor -> diBuilder.createBasicType(name.mangle(), sizeInBits)
        is Type.ParamRef -> requireUnreachable()
        is Type.TypeFunction -> requireUnreachable()
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> requireUnreachable()
        is Type.UntaggedUnion -> TODO()
        is Type.Uninferrable -> requireUnreachable()
        is Type.Ref -> requireUnreachable()
        is Type.AssociatedTypeRef -> requireUnreachable()
        is Type.Select -> requireUnreachable()
    }

    private fun lowerFunctionDef(definition: IRFunctionDef) {
//        logger().debug("LLVMGen::lowerFunctionDef(${definition.name.prettyPrint()})")
        val fn = getDeclaration(definition)
        currentFunction = fn
        definition.params.forEachIndexed { index, param ->
            localVariables[param.name] = fn.getParameter(index)
        }
        attachDebugInfo(definition, fn)
        lowerBlock(definition.entryBlock)
        for (block in definition.blocks) {
            lowerBlock(block)
        }
    }

    private var currentFunctionMetadata: LLVMMetadataRef? = null
    private fun attachDebugInfo(definition: IRFunctionDef, fn: FunctionValue) {

        if (!ctx.options.debugSymbols) {
            return
        }

        val fileScope = getFileScope(definition.location.file)
        val typeDI = LLVM.LLVMDIBuilderCreateSubroutineType(
            diBuilder, fileScope,
            definition.type.from.map { it.debugInfo }.asPointerPointer(),
            definition.type.from.size,
            LLVM.LLVMDIFlagZero
        )

        val name = definition.name.name.names.joinToString(".") { it.text }
        val meta = diBuilder.createFunction(
            scope = fileScope,
            name = name,
            linkageName = name,
            file = fileScope,
            lineno = definition.location.start.line,
            ty = typeDI,
            isLocalToUnit = false,
            isDefinition = true,
            scopeLine = definition.location.start.line,
            flags = LLVM.LLVMDIFlagZero,

            )
        currentFunctionMetadata = meta
        LLVM.LLVMGlobalSetMetadata(fn, LLVM.LLVMMDStringMetadataKind, meta)
        LLVM.LLVMSetCurrentDebugLocation2(builder, null)
    }

    private val fileScopeCache = mutableMapOf<SourcePath, LLVMMetadataRef>()
    private fun getFileScope(file: SourcePath): LLVMMetadataRef = fileScopeCache.getOrPut(file) {
        val f = diBuilder.createFile(file.path.fileName.toString(), file.path.toAbsolutePath().parent.toString())
        diBuilder.createCompileUnit(f)
        f
    }

    private val blocks = mutableMapOf<Pair<String, IRLocalName>, BasicBlock>()
    private fun getBlock(blockName: IRLocalName): BasicBlock {
        val fnName = requireNotNull(currentFunction?.getName())
        return blocks.computeIfAbsent(fnName to blockName) {
            requireNotNull(currentFunction).createBlock(it.second.mangle())
        }
    }

    private fun lowerBlock(block: IRBlock) {
        if (block.statements.isEmpty()) return
        val basicBlock = getBlock(block.name)
        builder.positionAtEnd(basicBlock)
        for (statement in block) {
            lowerStatement(statement)
        }
    }

    private fun lowerStatement(instruction: IRInstruction) = when (instruction) {
        is IRReturnInstruction -> lowerReturnStatement(instruction)
        is IRReturnVoidInstruction -> {
            builder.buildRetVoid()
            Unit
        }
        is IRValue -> {
            lowerExpression(instruction)
            Unit
        }
        is IRCall -> {
            lowerCallExpression(instruction)
            Unit
        }
        is IRAlloca -> lowerAlloca(instruction)
        is IRStore -> lowerStore(instruction)
        is IRLoad -> lowerLoad(instruction)
        is IRNot -> lowerNot(instruction)
        is IRBr -> lowerBr(instruction)
        is IRJump -> lowerJump(instruction)
        is IRBinOp -> lowerBinOp(instruction)
        is IRSwitch -> TODO()
    }

    private fun lowerBinOp(statement: IRBinOp) {
        val name = lowerName(statement.name)

        localVariables[statement.name] = if (isPredicateOperator(statement.operator)) {
            builder.buildICmp(
                    lowerPredicateOperator(statement.operator),
                    lowerExpression(statement.lhs),
                    lowerExpression(statement.rhs),
                    name
            )
        } else {
            builder.buildBinOp(
                    lowerOperator(statement.operator),
                    lowerExpression(statement.lhs),
                    lowerExpression(statement.rhs),
                    name)
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

    private fun lowerJump(statement: IRJump) {
        LLVM.LLVMBuildBr(builder.ref, getBlock(statement.label).ref)
    }

    private fun lowerBr(statement: IRBr) {
        builder.buildCondBr(lowerExpression(
            statement.condition),
            getBlock(statement.ifTrue),
            getBlock(statement.ifFalse))
    }

    private fun lowerNot(statement: IRNot) {
        val name = lowerName(statement.name)
        localVariables[statement.name] = builder.buildNot(lowerExpression(statement.arg), name)
    }

    private val localVariables = mutableMapOf<IRLocalName, Value>()

    private fun lowerAlloca(statement: IRAlloca) {
        val name = lowerName(statement.name)
        val ref = builder.buildAlloca(lowerType(statement.type), name,
            LLVM.LLVMABIAlignmentOfType(dataLayout, lowerType(statement.type)))
        localVariables[statement.name] = ref
    }


    private fun lowerStore(statement: IRStore) {
        val ptrType = statement.ptr.type
        require(ptrType is Type.Ptr)
        builder.buildStore(
            value = lowerExpression(statement.value),
            toPointer = lowerExpression(statement.ptr),
            alignment = sizeOfType(lowerType(statement.ptr.type)).toInt()
        )
    }

    private fun lowerLoad(statement: IRLoad) {
        val ref = builder.buildLoad(
            ptr = lowerExpression(statement.ptr),
            name = lowerName(statement.name)
        )
        localVariables[statement.name] = ref
    }

    private fun lowerReturnStatement(statement: IRReturnInstruction) {
        if (statement.value.type == Type.Void) {
            builder.buildRetVoid()
        } else {
            builder.buildRet(lowerExpression(statement.value))
        }
    }

    private fun lowerExpression(value: IRValue): Value {
        log.debug("lowerExpression(${value.prettyPrint()}) // ${value.location}")
        if (ctx.options.debugSymbols) {
            val location = LLVM.LLVMDIBuilderCreateDebugLocation(
                llvmCtx,
                value.location.start.line,
                value.location.start.column,
                currentFunctionMetadata,
                null
            )
            LLVM.LLVMSetCurrentDebugLocation2(builder, location)
        }

        val result = when (value) {
            is IRBool -> lowerBoolExpression(value)
            is IRByteString -> lowerByteString(value)
            is IRVariable -> lowerVariable(value)
            is IRGetStructField -> lowerGetStructField(value)
            is IRCIntConstant -> lowerCIntValue(value)
            is IRNullPtr -> lowerNullPtr(value)
            is IRSizeOf -> lowerSizeOf(value)
            is IRMethodRef -> requireUnreachable()
            is IRPointerCast -> lowerPointerCast(value)
            is IRAggregate -> lowerAggregate(value)
            is IRGetElementPointer -> lowerGetElementPointer(value)
            is IRUnsafeCast -> lowerUnsafeCast(value)
            is IRTruncate -> lowerTruncate(value)
            is IRZExt -> lowerZExt(value)
        }
        log.debug("Done lowering expression at ${value.location}")

        return result
    }

    private fun lowerZExt(value: IRZExt): Value {
        return builder.buildZExt(
            lowerExpression(value.value),
            lowerType(value.type),
            ctx.makeUniqueName().text,
        )
    }

    private fun lowerTruncate(value: IRTruncate): Value {
        return builder.buildTrunc(
            lowerExpression(value.value),
            lowerType(value.type),
            ctx.makeUniqueName().text
        )
    }

    private fun lowerGetElementPointer(value: IRGetElementPointer): Value {
        return builder.buildStructGEP(
                pointer = lowerExpression(value.ptr),
                name = ctx.makeUniqueName().text,
                index = value.offset
        )
    }

    private fun lowerAggregate(value: IRAggregate): Value {
        return ConstantStruct(
                lowerType(value.type),
                value.values.map { lowerExpression(it) }
        )
    }

    private fun lowerPointerCast(value: IRPointerCast): Value {
        val expr = lowerExpression(value.arg)
        return Value(
                LLVM.LLVMBuildPointerCast(
                        builder.ref,
                        expr.ref,
                        PointerType(lowerType(value.toPointerOfType)).ref,
                        ctx.makeUniqueName().text
                ))
    }

    private fun lowerUnsafeCast(value: IRUnsafeCast): Value {
        val fromType = lowerType(value.value.type)
        val toType = lowerType(value.type)
        val fromTypeSize = sizeOfType(fromType)
        val toTypeSize = sizeOfType(toType)
        return if (toTypeSize < fromTypeSize) {
            Value(
                LLVM.LLVMBuildTruncOrBitCast(
                    builder, lowerExpression(value.value), toType, ctx.makeUniqueName().text)
            )
        } else if (toTypeSize > fromTypeSize) {
            Value(
                LLVM.LLVMBuildZExt(
                    builder,
                    lowerExpression(value.value),
                    toType,
                    ctx.makeUniqueName().text
                )
            )
        } else {
            Value(
                LLVM.LLVMBuildBitCast(
                    builder, lowerExpression(value.value),
                    toType,
                    ctx.makeUniqueName().text
                )
            )
        }
    }

    private fun lowerSizeOf(value: IRSizeOf): Value {
        return Value(LLVM.LLVMSizeOf(lowerType(value.ofType).ref))
    }

    private fun lowerCIntValue(value: IRCIntConstant): Value {
        return ConstantInt(value = value.value.toLong(), type = lowerType(value.type), signExtend = false)
    }

    private fun lowerNullPtr(value: IRNullPtr): Value {
        return lowerType(value.type).getConstantNullPointer()
    }

    private fun lowerGetStructField(expression: IRGetStructField): Value {
        return builder.buildExtractValue(
            lowerExpression(expression.lhs),
            expression.index,
            ctx.makeUniqueName().text
        )
    }

    private fun lowerBoolExpression(expression: IRBool): Value {
        return if (expression.value) {
            trueValue
        } else {
            falseValue
        }
    }

    private fun lowerVariable(expression: IRVariable): Value {
        log.debug("lowerVariable(${expression.prettyPrint()}) // ${expression.location}")
        return when (expression.name) {
            is IRLocalName -> requireNotNull(localVariables[expression.name]) {
                "${expression.location}: Unbound variable: ${expression.name.prettyPrint()}"
            }
            is IRGlobalName -> lowerGlobalVariable(expression.name)
        }
    }


    private fun lowerGlobalVariable(name: IRGlobalName) = when (val binding = irModule.resolveGlobal(name)) {
        is IRBinding.FunctionDef -> getDeclaration(binding.def)
        is IRBinding.ExternFunctionDef -> getDeclaration(binding.def)
        is IRBinding.StructDef -> getStructConstructor(binding.def)
        is IRBinding.ConstDef -> getConstDefValue(binding.def)
        is IRBinding.ExternConstDef -> getExternConstRef(binding.def)
    }

    private fun lowerByteString(expression: IRByteString): Value {
        val text = expression.value.decodeToString()
        val constStringRef = ConstantArray(text, nullTerminate = false, context = llvmCtx)
        val globalRef = llvmModule.addGlobal(
            stringLiteralName(),
            constStringRef.getType()
        )
        globalRef.setInitializer(constStringRef)
        return Value(LLVM.LLVMConstPointerCast(globalRef.ref, bytePtrTy.ref))
    }

    private fun lowerCallExpression(expression: IRCall): Value {
        log.debug("lowerCallExpression(${expression.location})")
        val callee = lowerExpression(expression.callee)
        require(expression.typeArgs == null) { "Unspecialized generic function found in LLVMGen" }
        val args = expression.args.map { lowerExpression(it) }
        val ref = builder.buildCall(callee, args, if (expression.type == Type.Void) null else expression.name.mangle())

        localVariables[expression.name] = ref

        return ref
    }

    private fun withinBlock(basicBlock: BasicBlock, fn: Builder.() -> Unit) {
        builder.positionAtEnd(basicBlock)
        builder.fn()
    }


    private fun getDeclaration(externFunctionDef: IRExternFunctionDef): FunctionValue {
        val irName = externFunctionDef.externName
        val type = lowerFunctionType(externFunctionDef.type)
        val name = if (irName.text == "main") {
            "hades_main"
        } else {
            irName.text
        }
        return llvmModule.getFunction(name)?.asFunctionValue() ?: llvmModule.addFunction(name, type)
    }

    private fun getDeclaration(def: IRFunctionDef): FunctionValue {
        require(def.signature.constraints.isEmpty())
        val irName = def.name
        val type = lowerFunctionType(def.type)
        val name = if (irName.mangle() == "main") {
            "hades_main"
        } else {
            lowerName(irName)
        }
        return (llvmModule.getFunction(name) ?: llvmModule.addFunction(name, type)).asFunctionValue()
    }

    private fun getStructConstructor(def: IRStructDef): FunctionValue {
        val name = lowerName(def.globalName)
        val existing = llvmModule.getFunction(name)?.asFunctionValue()
        return if (existing != null) {
            existing
        } else {
            val constructorType = def.constructorType
            require(constructorType is Type.Function)
            val loweredType = lowerFunctionType(constructorType)
            llvmModule.addFunction(name, loweredType)
        }


    }


    private fun lowerFunctionType(type: Type): llvm.Type {
        val lowered = lowerType(type)
        return lowered
    }

    private fun lowerName(name: IRName): String {
        return name.mangle()
    }

    private fun verifyModule() {
        // TODO: Handle this in a better way
        val buffer = ByteArray(1000)
        val error = LLVM.LLVMVerifyModule(llvmModule.ref, LLVM.LLVMAbortProcessAction, buffer)
        require(error == 0) {
            log.error(LLVM.LLVMPrintModuleToString(llvmModule.ref).string)
            log.error("Invalid llvm module: ${llvmModule.getSourceFileName()}\n")
            "Invalid LLVM module ${String(buffer.sliceArray(0..error))}"
        }
    }

    private fun LLVMValueRef.verify() {
        val validate = LLVM.LLVMVerifyFunction(ref, LLVM.LLVMPrintMessageAction)
        if (validate > 0) {
            log.error("Bad function: ${this.dumpToString()}")
            TODO()
        }
    }

    private val structTypes = mutableMapOf<QualifiedName, llvm.Type>()
    private fun lowerType(type: Type): llvm.Type = when (type) {
        is Type.Error -> requireUnreachable {
            "${type.location}"
        }
        Type.Byte -> byteTy
        Type.Void -> voidTy
        is Type.Bool -> boolTy
        Type.Double -> doubleTy
        is Type.Ptr -> ptrTy(lowerType(type.to))
        is Type.Function -> {
            FunctionType(
                returns = lowerType(type.to),
                types = type.from.map { lowerType(it) },
                variadic = false
            )
        }
        is Type.Constructor ->  {
            if (type.name !in structTypes) {
                val binding = irModule.resolveGlobal(type.name)
                require(binding is IRBinding.StructDef)
                val memberTypes = binding.def.fields
                val name = type.name.mangle()
                val structTy = StructType(name, llvmCtx)
                structTypes[type.name] = structTy
                structTy.setBody(memberTypes.values.map { lowerType(it) }, packed = false)
                structTy
            } else {
                requireNotNull(structTypes[type.name])
            }
        }
        is Type.ParamRef ->
            TODO("Can't lower unspecialized type param")
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> requireUnreachable()
        Type.Size -> sizeTy
        is Type.UntaggedUnion -> {
            val maxSizedType = type.members
                    .map { lowerType(it) }
                    .maxByOrNull { sizeOfType(it) }
            requireNotNull(maxSizedType)
        }
        is Type.Integral -> IntType(type.size, llvmCtx)
        is Type.FloatingPoint -> FloatType(type.size, llvmCtx)
        is Type.TypeFunction -> requireUnreachable()
        is Type.Uninferrable -> requireUnreachable()
        is Type.Ref -> PointerType(lowerType(type.to))
        is Type.AssociatedTypeRef -> requireUnreachable()
        is Type.Select -> requireUnreachable()
    }

    private var nextLiteralIndex = 0
    private fun stringLiteralName(): String {
        nextLiteralIndex++
        return "_hadesboot_string_literal_$nextLiteralIndex"
    }

    private val byteTy = IntType(8, llvmCtx)
    private val bytePtrTy = PointerType(byteTy)
    private val voidTy = VoidType(llvmCtx)
    private val boolTy = IntType(1, llvmCtx)
    private val cIntTy = IntType(32, llvmCtx)
    private val i32Ty = IntType(32, llvmCtx)
    private val doubleTy = LLVM.LLVMDoubleTypeInContext(llvmCtx)
    private val trueValue = ConstantInt(boolTy, 1, false)
    private val falseValue = ConstantInt(boolTy, 0, false)
    private val sizeTy = IntType(64, llvmCtx) // FIXME: This isn't portable

    private fun ptrTy(to: llvm.Type): llvm.Type {
        return PointerType(to)
    }

    private fun linkWithRuntime() = profile("LLVMGen::linkWithRuntime") {
        val cc = System.getenv("CC") ?: if (SystemUtils.IS_OS_MAC_OSX) "clang" else "gcc"
        log.info("Linking using $cc")
        val commandParts = mutableListOf(
            cc,
        )
        if (ctx.options.debugSymbols) {
            commandParts.add("-g")
        } else {
            if (!SystemUtils.IS_OS_WINDOWS) {
                commandParts.add("-flto")
            }
            commandParts.add("-O2")
        }
        commandParts.add("-o")
        commandParts.add(ctx.options.output.toString())
        commandParts.addAll(ctx.options.cSources.map { it.toString() })
        commandParts.add(ctx.options.runtime.toString())
        commandParts.add(objectFilePath)
        commandParts.addAll(ctx.options.cFlags)
        commandParts.addAll(ctx.options.libs.map { "-l$it" })

        val outputFile = ctx.options.output.toFile()
        if (outputFile.exists()) {
            outputFile.delete()
        }
        log.info(commandParts.joinToString(" "))
        val builder = ProcessBuilder(commandParts)
        val process = builder
            .inheritIO()
            .start()
        val exitCode = process.waitFor()
        require(exitCode == 0) {
            "${commandParts.joinToString(" ")} exited with code $exitCode"
        }
    }

    private val objectFilePath get() = ctx.options.output.toString() + ".o"

    private fun writeModuleToFile() {
        log.debug("Writing object file")
        LLVM.LLVMInitializeAllTargetInfos()
        LLVM.LLVMInitializeAllTargets()
        LLVM.LLVMInitializeAllTargetMCs()
        LLVM.LLVMInitializeAllAsmParsers()
        LLVM.LLVMInitializeAllAsmPrinters()
        val targetTriple = LLVM.LLVMGetDefaultTargetTriple()
        val cpu = BytePointer("generic")
        val features = BytePointer("")
        val target = LLVM.LLVMGetFirstTarget()
        val targetMachine: LLVMTargetMachineRef = LLVM.LLVMCreateTargetMachine(
            target,
            targetTriple,
            cpu,
            features,
            LLVM.LLVMCodeGenLevelLess,
            LLVM.LLVMRelocPIC,
            LLVM.LLVMCodeModelDefault
        )

        if (!ctx.options.debugSymbols) {

            val pass = LLVM.LLVMCreatePassManager()
            LLVM.LLVMAddConstantPropagationPass(pass)
            LLVM.LLVMAddFunctionInliningPass(pass)
            LLVM.LLVMAddPromoteMemoryToRegisterPass(pass)
            LLVM.LLVMAddAggressiveDCEPass(pass)
            LLVM.LLVMAddFunctionInliningPass(pass)
            LLVM.LLVMAddGlobalDCEPass(pass)
            LLVM.LLVMAddGlobalOptimizerPass(pass)
            LLVM.LLVMRunPassManager(pass, llvmModule.ref)
            LLVM.LLVMDisposePassManager(pass)
        }

        if (ctx.options.dumpLLVMModule) {
            LLVM.LLVMPrintModuleToFile(llvmModule, "$objectFilePath.ll", null as BytePointer?)
        }


        LLVM.LLVMTargetMachineEmitToFile(
            targetMachine,
            llvmModule.ref,
            BytePointer(objectFilePath),
            LLVM.LLVMObjectFile,
            BytePointer("Message")
        )

        LLVM.LLVMDisposeTargetMachine(targetMachine)
    }

    override fun close() {
        llvmCtx.dispose()
    }
}
