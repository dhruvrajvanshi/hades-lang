package hadesc.hirgen

import hadesc.Name
import hadesc.analysis.TypeAnalyzer
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.context.NamingContext
import hadesc.defer
import hadesc.diagnostics.Diagnostic
import hadesc.frontend.PropertyBinding
import hadesc.hir.*
import hadesc.hir.HIRStatement.Companion.ifStatement
import hadesc.ignore
import hadesc.location.HasLocation
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.scoped
import hadesc.types.Type
import hadesc.types.mutPtr
import hadesc.types.ptr
import libhades.collections.Stack
import java.nio.file.Path

internal interface HIRGenModuleContext : TypeTransformer {
    val enumTagFieldName: Name
    val currentModule: HIRModule

    fun lowerGlobalName(binder: Binder): QualifiedName
    fun typeOfExpression(expression: Expression): Type
    fun lowerLocalBinder(binder: Binder): Name
    fun lowerFunctionDef(functionDef: Declaration.FunctionDef, qualifiedName: QualifiedName? = null): HIRDefinition.Function
    fun lowerTypeParam(typeParam: TypeParam): HIRTypeParam
    fun lowerTypeAnnotation(annotation: TypeAnnotation): Type

    /**
     * FIXME: It would be a better design to take binder name as the parameter so that
     *        callers don't have to know about ExternFunctionDef.
     */
    fun getExternDef(declaration: Declaration.ExternFunctionDef): HIRDefinition.ExternFunction
}

internal interface HIRGenFunctionContext : HIRBuilder {
    val scopeStack: HIRGenScopeStack
    fun lowerExpression(expression: Expression): HIRExpression
    fun HIRExpression.asOperand(): HIROperand
    fun lowerBlock(
        body: Block,
        addReturnVoid: Boolean = false,
        into: HIRBlock = HIRBlock(currentLocation, namingCtx.makeUniqueName(), mutableListOf()),
        before: HIRBuilder.() -> Unit = {},
        after: HIRBuilder.() -> Unit = {}
    ): HIRBlock
}

internal class ScopeMeta

class HIRGenScopeStack {
    private val stack = mutableListOf<Pair<ScopeTree, ScopeMeta>>()
    fun push(scope: ScopeTree) {
        stack.add(Pair(scope, ScopeMeta()))
    }

    fun pop(): ScopeTree {
        check(stack.isNotEmpty())
        return stack.removeLast().first
    }
}
class HIRGen(private val ctx: Context, private val typeTransformer: HIRGenTypeTransformer = HIRGenTypeTransformer(ctx)) : ASTContext by ctx, HIRGenModuleContext, HIRGenFunctionContext, NamingContext by ctx, HIRBuilder, TypeTransformer by typeTransformer {

    private val log = logger(HIRGen::class.java)
    override val typeAnalyzer = TypeAnalyzer()
    override val namingCtx: NamingContext get() = ctx
    override val enumTagFieldName = ctx.makeName("\$tag")
    private val enumPayloadFieldName = ctx.makeName("payload")
    override val currentModule = HIRModule(mutableListOf())
    private val closureGen = HIRGenClosure(
        ctx,
        moduleContext = this,
        functionContext = this
    )

    private val exprGen = HIRGenExpression(
        ctx,
        moduleContext = this,
        functionContext = this,
        closureGen = closureGen
    )
    private val traitGen = HIRGenTraits(ctx, this)
    override lateinit var currentLocation: SourceLocation
    override var currentStatements: MutableList<HIRStatement>? = null
    override val scopeStack = HIRGenScopeStack()

    @Suppress("TooGenericExceptionCaught") // TODO:
    fun lowerSourceFiles(sourceFiles: Collection<SourceFile>): HIRModule {
        val declarations = currentModule.definitions
        val builtinLoc = SourceLocation(
            SourcePath(Path.of("hades_prelude.hds")),
            Position(0, 0),
            Position(0, 0)
        )
        currentModule.definitions.add(
            HIRDefinition.ExternFunction(
                builtinLoc,
                externName = ctx.makeName("_hdc_puts"),
                name = qn("_hdc_puts"),
                returnType = Type.Void,
                params = listOf(Type.u8.ptr())
            )

        )
        val inputDeclarations = mutableListOf<Declaration>()
        try {
            for (sourceFile in sourceFiles) scoped {
                currentLocation = sourceFile.location
                scopeStack.push(sourceFile)
                defer { check(scopeStack.pop() === sourceFile) }

                inputDeclarations.addAll(sourceFile.declarations)
            }
            val decls = inputDeclarations
                .sortedBy {
                    when (it) {
                        is Declaration.Struct,
                        is Declaration.Enum
                        -> 1
                        else -> 2
                    }
                }
            decls.forEach { declaration ->
                declarations.addAll(lowerDeclaration(declaration))
            }
            declarations.addAll(externDefs.values)
            val result = HIRModule(declarations)
            log.debug("HIRGen output")
            log.debug(result.prettyPrint())
            return result
        } catch (e: Error) {
            log.error("Error while compiling module", e)
            log.debug("Generated sources:\n${declarations.joinToString("\n\n") { it.prettyPrint() }}")
            log.debug("current statements:\n${currentStatements?.joinToString("\n") { it.prettyPrint() }}")
            throw e
        }
    }

    private fun lowerDeclaration(declaration: Declaration): List<HIRDefinition> {
        currentLocation = declaration.location
        return lowerDeclarationHelper(declaration)
    }
    private fun lowerDeclarationHelper(declaration: Declaration): List<HIRDefinition> = when (declaration) {
        is Declaration.Error -> requireUnreachable()
        is Declaration.ImportAs -> emptyList()
        is Declaration.FunctionDef -> listOf(lowerFunctionDef(declaration))
        is Declaration.ConstDefinition -> lowerConstDef(declaration)
        is Declaration.ExternFunctionDef -> emptyList()
        is Declaration.Struct -> lowerStructDef(declaration)
        is Declaration.TypeAlias -> emptyList()
        is Declaration.ExtensionDef -> lowerExtensionDef(declaration)
        is Declaration.TraitDef -> traitGen.lowerTraitDef(declaration)
        is Declaration.ImplementationDef -> traitGen.lowerImplementationDef(declaration)
        is Declaration.ImportMembers -> emptyList()
        is Declaration.Enum -> lowerEnumDef(declaration)
        is Declaration.ExternConst -> lowerExternConstDef(declaration)
    }

    private fun lowerExternConstDef(declaration: Declaration.ExternConst): List<HIRDefinition> {
        return listOf(
            HIRDefinition.ExternConst(
                declaration.location,
                lowerGlobalName(declaration.name),
                lowerTypeAnnotation(declaration.type),
                declaration.externName.name
            )
        )
    }

    private var currentExtensionDef: Declaration.ExtensionDef? = null
    private fun lowerExtensionDef(declaration: Declaration.ExtensionDef): List<HIRDefinition> {
        require(currentExtensionDef == null)
        currentExtensionDef = declaration
        val list = mutableListOf<HIRDefinition>()
        for (functionDef in declaration.functionDefs) {
            list.add(
                lowerFunctionDef(
                    functionDef,
                    extensionMethodName(declaration, functionDef)
                )
            )
        }
        currentExtensionDef = null
        return list
    }

    private fun extensionMethodName(declaration: Declaration.ExtensionDef, functionDef: Declaration.FunctionDef): QualifiedName {
        return ctx.resolver.qualifiedName(declaration.name).append(functionDef.name.identifier.name)
    }

    private fun lowerConstDef(declaration: Declaration.ConstDefinition): List<HIRDefinition> {
        return listOf(
            HIRDefinition.Const(
                declaration.location,
                lowerGlobalName(declaration.name),
                lowerExpression(declaration.initializer)
            )
        )
    }

    private fun lowerStructDef(declaration: Declaration.Struct): List<HIRDefinition> {
        val fields = declaration.members.map {
            require(it is Declaration.Struct.Member.Field)
            it.binder.identifier.name to lowerTypeAnnotation(it.typeAnnotation)
        }
        return listOf(
            HIRDefinition.Struct(
                declaration.location,
                lowerGlobalName(declaration.binder),
                typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                fields = fields
            )
        )
    }

    private fun lowerEnumDef(declaration: Declaration.Enum): List<HIRDefinition> {
        val enumName = lowerGlobalName(declaration.name)
        val enumStructDef = emitDef(
            HIRDefinition.Struct(
                declaration.location,
                lowerGlobalName(declaration.name),
                typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                fields = listOf(
                    enumTagFieldName to ctx.enumTagType(),
                    enumPayloadFieldName to ctx.analyzer.getEnumPayloadType(declaration)
                )
            )
        )

        declaration.cases.forEachIndexed { index, case ->
            val enumCaseStructDef = emitDef(
                HIRDefinition.Struct(
                    case.name.location,
                    enumName.append(case.name.identifier.name),
                    typeParams = declaration.typeParams?.map { HIRTypeParam(it.location, it.binder.identifier.name, it.binder.id) },
                    fields = (
                        case.params?.mapIndexed { caseParamIndex, caseParam ->
                            ctx.makeName("$caseParamIndex") to lowerTypeAnnotation(requireNotNull(caseParam.annotation))
                        } ?: emptyList()
                        )
                )
            )
            val caseTagDef = emitDef(
                HIRDefinition.Const(
                    case.name.location,
                    caseTagName(enumName, case),
                    HIRConstant.IntValue(
                        case.name.location,
                        ctx.enumTagType(),
                        index
                    )
                )
            )

            emitDef(
                enumCaseConstructor(
                    enumStructDef,
                    enumCaseStructDef,
                    caseTagDef,
                    case
                )
            )
        }

        return emptyList()
    }

    private fun caseTagName(enumName: QualifiedName, case: Declaration.Enum.Case): QualifiedName {
        return enumName.append(case.name.identifier.name).append(ctx.makeName("tag"))
    }

    private fun enumCaseConstructor(
        enumStructDef: HIRDefinition.Struct,
        enumCaseStructDef: HIRDefinition.Struct,
        caseTagDef: HIRDefinition.Const,
        case: Declaration.Enum.Case
    ): HIRDefinition.Function {
        val constructorParams = case.params?.mapIndexed { index, param ->
            HIRParam(
                param.annotation.location,
                Binder(
                    Identifier(param.annotation.location, ctx.makeName("param_$index")),
                    param.binder?.id ?: ctx.makeBinderId(),
                ),
                lowerTypeAnnotation(checkNotNull(param.annotation))
            )
        } ?: emptyList()
        val fnName = enumCaseStructDef.name.append(ctx.makeName("constructor"))
        val typeArgs = enumStructDef.typeParams?.map { Type.Param(it.toBinder()) } ?: emptyList()
        val instanceType = lowerType(enumStructDef.instanceType(typeArgs))
        val payloadType = lowerType(enumCaseStructDef.instanceType(typeArgs))
        val body = buildBlock(case.name.location, ctx.makeName("entry")) {
            currentLocation = case.name.location
            val resultRef = emitAlloca("result", instanceType)
            val resultPtr = resultRef.ptr()
            emitStore(
                resultPtr.fieldPtr(enumTagFieldName),
                caseTagDef.ref()
            )
            val payloadPtr = resultPtr
                .fieldPtr(enumPayloadFieldName, ctx.makeUniqueName("payload_union_ptr"))
                .ptrCast(payloadType, "payload_ptr")

            constructorParams.forEachIndexed { index, param ->
                val paramPtr = payloadPtr.fieldPtr(index.toString())
                val paramRef = param.ref()

                emitStore(paramPtr, paramRef)
            }

            emitReturn(resultRef.ptr().load())
        }

        return HIRDefinition.Function(
            case.name.location,
            HIRFunctionSignature(
                case.name.location,
                fnName,
                enumStructDef.typeParams,
                params = constructorParams,
                returnType = instanceType
            ),
            mutableListOf(body)
        )
    }

    private var currentFunctionDef: Declaration.FunctionDef? = null
    override fun lowerFunctionDef(
        functionDef: Declaration.FunctionDef,
        qualifiedName: QualifiedName?
    ): HIRDefinition.Function = scoped {
        check(currentFunctionDef == null)
        currentFunctionDef = functionDef
        defer {
            check(currentFunctionDef === functionDef)
            currentFunctionDef = null
        }

        scopeStack.push(functionDef)
        defer { check(scopeStack.pop() === functionDef) }

        val signature = lowerFunctionSignature(functionDef.signature, qualifiedName)
        val returnType = signature.returnType
        val addReturnVoid = returnType is Type.Void && !hasTerminator(functionDef.body)
        val body = lowerBlock(
            functionDef.body,
            addReturnVoid
        ).copy(name = ctx.makeName("entry"))
        HIRDefinition.Function(
            location = functionDef.location,
            signature = signature,
            mutableListOf(body)
        )
    }

    private fun lowerFunctionSignature(
        signature: FunctionSignature,
        qualifiedName: QualifiedName? = null
    ): HIRFunctionSignature {
        val returnType = lowerTypeAnnotation(signature.returnType)
        val name = qualifiedName ?: lowerGlobalName(signature.name)
        val params = mutableListOf<HIRParam>()
        if (signature.thisParamFlags != null) {
            require(qualifiedName != null)
            params.add(
                HIRParam(
                    signature.location,
                    binder = checkNotNull(signature.thisParamBinder),
                    type = thisParamType()
                )
            )
        }
        params.addAll(signature.params.map { lowerParam(it) })

        var typeParams: MutableList<HIRTypeParam>? = mutableListOf()
        val extensionDef = currentExtensionDef
        if (extensionDef != null) {
            extensionDef.typeParams?.map { lowerTypeParam(it) }?.let {
                typeParams?.addAll(it)
            }
        }
        signature.typeParams?.map { lowerTypeParam(it) }?.let {
            typeParams?.addAll(it)
        }
        if (extensionDef?.typeParams == null && signature.typeParams == null) {
            typeParams = null
        }

        return HIRFunctionSignature(
            location = signature.location,
            name = name,
            typeParams = typeParams,
            params = params,
            returnType = returnType
        )
    }

    private var deferStack = Stack<MutableList<HIRStatement>>()
    override fun lowerBlock(
        body: Block,
        addReturnVoid: Boolean,
        into: HIRBlock,
        before: HIRBuilder.() -> Unit,
        after: HIRBuilder.() -> Unit
    ): HIRBlock = scoped {
        scopeStack.push(body)
        defer { check(scopeStack.pop() === body) }
        buildBlock(body.location, into = into) {
            deferStack.push(mutableListOf())
            before()
            for (member in body.members) {
                lowerBlockMember(member)
            }
            after()
            if (addReturnVoid) {
                terminateScope()
                emitReturn(HIRConstant.Void(body.location))
            } else {
                for (statement in requireNotNull(deferStack.peek())) {
                    emit(statement)
                }
            }
            deferStack.pop()
        }
    }

    private fun lowerBlockMember(member: Block.Member): Unit = when (member) {
        is Block.Member.Expression ->
            allocaAssign(
                ctx.makeUniqueName(),
                lowerExpression(member.expression)
            ).ignore()
        is Block.Member.Statement -> lowerStatement(member.statement)
    }

    private fun lowerStatement(statement: Statement): Unit = when (statement) {
        is Statement.Return -> lowerReturnStatement(statement)
        is Statement.Val -> lowerValStatement(statement)
        is Statement.While -> lowerWhileStatement(statement)
        is Statement.If -> lowerIfStatement(statement)
        is Statement.LocalAssignment -> lowerLocalAssignment(statement)
        is Statement.MemberAssignment -> lowerMemberAssignmentStatement(statement)
        is Statement.PointerAssignment -> lowerPointerAssignment(statement)
        is Statement.Defer -> lowerDeferStatement(statement)
        is Statement.Error -> requireUnreachable()
    }

    private fun lowerDeferStatement(statement: Statement.Defer) {
        val statements = mutableListOf<HIRStatement>()
        intoStatementList(statements) {
            lowerBlockMember(statement.blockMember)
        }

        requireNotNull(deferStack.peek()).addAll(statements.reversed())
    }

    private fun lowerMemberAssignmentStatement(statement: Statement.MemberAssignment) {
        val lhsType = statement.lhs.lhs.type
        if (ctx.analyzer.isRefStructType(lhsType)) {
            lowerRefStructMemberAssignment(statement)
            return
        }
        when (statement.lhs.lhs) {
            is Expression.Var -> {
                val binding = ctx.resolver.resolve(statement.lhs.lhs.name)
                require(binding is Binding.ValBinding)
                require(binding.statement.isMutable)
                val fieldAddr = addressOfStructField(statement.lhs)
                emit(
                    HIRStatement.Store(
                        location = statement.location,
                        ptr = fieldAddr,
                        value = lowerExpression(statement.value)
                    )
                )
            }
            is Expression.This -> {
                val enclosingExtension = checkNotNull(ctx.resolver.getEnclosingFunction(statement))
                val thisFlags = checkNotNull(enclosingExtension.signature.thisParamFlags)
                check(thisFlags.isPointer)
                check(thisFlags.isMutable)

                val thisValue = lowerThisExpression(statement.lhs.lhs)
                emitStore(
                    thisValue
                        .fieldPtr(statement.lhs.property.name),
                    lowerExpression(statement.value)
                )
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerRefStructMemberAssignment(statement: Statement.MemberAssignment) {
        val ref = lowerExpression(statement.lhs.lhs)
        ref.storeRefField(
            statement.lhs.property.name,
            lowerExpression(statement.value)
        )
    }

    private fun lowerPointerAssignment(statement: Statement.PointerAssignment) {
        val ptr = lowerExpression(statement.lhs.expression)
        emit(
            HIRStatement.Store(
                statement.location,
                ptr = ptr,
                value = lowerExpression(statement.value)
            )
        )
    }

    private fun lowerLocalAssignment(statement: Statement.LocalAssignment) {
        val binding = ctx.resolver.resolve(statement.name)
        require(binding is Binding.ValBinding)
        if (ctx.analyzer.isClosureCapture(statement.name)) {
            closureGen.lowerCaptureAssignment(statement)
            return
        }
        emitStore(
            HIRExpression.LocalRef(
                currentLocation,
                typeOfExpression(statement.value).mutPtr(),
                binding.statement.binder.name
            ),
            lowerExpression(statement.value)
        )
    }

    private fun lowerWhileStatement(statement: Statement.While) {
        currentLocation = statement.condition.location
        val conditionVar = emitAlloca("while_condition", Type.Bool)
        emit(
            HIRStatement.While(
                statement.location,
                conditionName = conditionVar.name,
                buildBlock(statement.condition.location, ctx.makeUniqueName("while_condition_block")) {
                    emitStore(
                        conditionVar.mutPtr(),
                        lowerExpression(statement.condition)
                    )
                },
                lowerBlock(statement.body)
            )
        )
    }

    private val valAllocaStatements = mutableMapOf<Binder, HIRStatement.Alloca>()
    private fun lowerValStatement(statement: Statement.Val) {
        check(valAllocaStatements[statement.binder] == null)
        val name = lowerLocalBinder(statement.binder)
        valAllocaStatements[statement.binder] =
            // this condition isn't necessary,
            // it's just to create cleaner HIR for val x: Type = #uninitialized
            if (statement.rhs is Expression.Uninitialized) {
                emitAlloca(name, statement.rhs.type)
                // the other branch would generate this code
                // __generated__: Type = alloca Type
                // x: *mut Type = alloca Type
                // store x = *__generated__
            } else {
                allocaAssign(name, lowerExpression(statement.rhs))
            }
    }

    override fun lowerLocalBinder(binder: Binder): Name {
        // TODO: Handle shadowing
        return binder.identifier.name
    }

    private fun lowerIfStatement(statement: Statement.If) {
        emit(
            ifStatement(
                location = statement.location,
                condition = lowerExpression(statement.condition),
                trueBranch = lowerBlock(statement.ifTrue),
                falseBranch = statement.ifFalse?.let { lowerBlock(it) } ?: buildBlock {}
            )
        )
    }

    private fun lowerReturnStatement(statement: Statement.Return) {
        terminateScope()
        requireNotNull(deferStack.peek()).removeIf { true }
        if (statement.value == null) {
            emitReturn(HIRConstant.Void(statement.location))
        } else {
            emitReturn(lowerExpression(statement.value))
        }
    }

    private fun terminateScope() {
        for (deferStatements in deferStack) {
            for (statement in deferStatements.reversed()) {
                emit(statement)
            }
        }
    }

    override fun lowerExpression(expression: Expression): HIRExpression {
        currentLocation = expression.location
        val lowered = when (expression) {
            is Expression.Error -> requireUnreachable()
            is Expression.Var -> exprGen.lowerVarExpression(expression)
            is Expression.Call -> exprGen.lowerCallExpression(expression)
            is Expression.Property -> lowerPropertyExpression(expression)
            is Expression.ByteString -> lowerByteString(expression)
            is Expression.BoolLiteral -> lowerBoolLiteral(expression)
            is Expression.NullPtr -> lowerNullPtr(expression)
            is Expression.IntLiteral -> lowerIntLiteral(expression)
            is Expression.Not -> lowerNotExpression(expression)
            is Expression.BinaryOperation -> lowerBinaryExpression(expression)
            is Expression.SizeOf -> lowerSizeOfExpression(expression)
            is Expression.AddressOf -> lowerAddressOfExpression(expression)
            is Expression.AddressOfMut -> lowerAddressOfMut(expression)
            is Expression.Deref -> lowerDerefExpression(expression)
            is Expression.PointerCast -> lowerPointerCast(expression)
            is Expression.If -> lowerIfExpression(expression)
            is Expression.TypeApplication -> lowerTypeApplication(expression)
            is Expression.This -> lowerThisExpression(expression)
            is Expression.Closure -> {
                val oldDeferStack = deferStack
                deferStack = Stack()
                val result = closureGen.lowerClosure(expression)
                deferStack = oldDeferStack
                result
            }
            is Expression.As -> lowerAsExpression(expression)
            is Expression.BlockExpression -> lowerBlockExpression(expression)
            is Expression.Intrinsic -> requireUnreachable()
            is Expression.UnaryMinus -> lowerUnaryMinus(expression)
            is Expression.ByteCharLiteral -> lowerByteCharExpression(expression)
            is Expression.Match -> lowerMatchExpression(expression)
            is Expression.FloatLiteral -> lowerFloatLiteral(expression)
            is Expression.Uninitialized -> lowerUninitialized(expression)
            is Expression.Move -> lowerMoveExpression(expression)
            is Expression.AlignOf -> lowerAlignOfExpression(expression)
        }
        val typeArgs = ctx.analyzer.getTypeArgs(expression)
        val exprType = lowered.type
        checkUninferredGenerics(expression, exprType)
        val withAppliedTypes = if (typeArgs != null) {
            check(exprType is Type.TypeFunction)
            check(exprType.params.size == typeArgs.size)
            emitTypeApplication(
                lowered,
                typeArgs.map { ctx.analyzer.reduceGenericInstances(it) }
            ).result()
        } else {
            check(exprType !is Type.TypeFunction) {
                "${expression.location}"
            }
            postLowerExpression(lowered)
        }

        val enumConstructorBinding = ctx.analyzer.getEnumConstructorBinding(expression)
        return if (enumConstructorBinding != null && expression !is Expression.TypeApplication) {
            /**
             * Implicitly call zero argument constructor for
             * enum case with zero parameters.
             * see [lowerEnumCaseConstructor]
             */
            if (enumConstructorBinding.case.params == null) {
                emitCall(
                    withAppliedTypes,
                    emptyList()
                ).result()
            } else {
                withAppliedTypes
            }
        } else {
            withAppliedTypes
        }
    }

    private fun lowerAlignOfExpression(expression: Expression.AlignOf): HIROperand {
        return HIRConstant.AlignOf(
            expression.location,
            type = Type.usize,
            ofType = lowerTypeAnnotation(expression.type)
        )
    }

    private fun lowerMoveExpression(expression: Expression.Move): HIROperand {
        val binding = ctx.resolver.resolve(expression.name)
        check(binding != null)

        // the basic idea here is this.
        // This is the last time we should allow the use of this name
        // Emitting a move instruction directly here doesn't work because
        // that will flag this use as well.
        // so we just create a new variable for this use, copy the original variable into
        // the new one and then emit a move instruction

        // / foo(move x, x)
        // / This should emit the following:
        // /
        // / moveFrom = alloca typeof x
        // / store moveFrom = x
        // / move x
        // / foo(
        //      moveFrom, // this is fine
        //      x,  // this is a move after use
        //      )
        val moveFrom = exprGen.lowerBinding(expression, binding)
        val alloca = emitAlloca(namePrefix = expression.name.name.text + "_moved", expression.type)
        emitStore(alloca.mutPtr(), moveFrom)
        emit(HIRStatement.Move(currentLocation, binding.binder.name))

        return alloca.ptr().load()
    }

    private fun lowerUninitialized(expression: Expression.Uninitialized): HIROperand {
        val ofType = typeOfExpression(expression)
        return emitAlloca(ctx.makeUniqueName(), ofType).ptr().load()
    }

    private fun lowerFloatLiteral(expression: Expression.FloatLiteral): HIROperand {
        val ty = expression.type
        check(ty is Type.FloatingPoint)
        return HIRConstant.FloatValue(
            expression.location,
            ty,
            expression.value
        )
    }

    private fun lowerMatchExpression(expression: Expression.Match): HIRExpression {
        val discriminantType = typeOfExpression(expression.value)
        if (discriminantType.isIntegral()) {
            return lowerIntegralMatchExpression(expression)
        }

        return exprGen.lowerEnumMatchExpression(expression)
    }

    private fun lowerIntegralMatchExpression(expression: Expression.Match): HIRExpression {
        val resultType = typeOfExpression(expression)
        val discriminantType = typeOfExpression(expression.value)
        require(discriminantType.isIntegral())
        val result = emitAlloca("result", resultType)
        val arms = expression.arms.takeWhile { it.pattern !is Pattern.Wildcard }
        val elseArm = expression.arms.find { it.pattern is Pattern.Wildcard }
        check(elseArm != null)
        val elseBlockName = ctx.makeUniqueName()

        emit(
            HIRStatement.MatchInt(
                expression.location,
                lowerExpression(expression.value),
                arms.map { arm ->
                    check(arm.pattern is Pattern.IntLiteral)

                    val blockName = ctx.makeUniqueName()
                    MatchIntArm(
                        HIRConstant.IntValue(arm.location, discriminantType, arm.pattern.value.toInt()),
                        buildBlock(arm.location, blockName) {
                            emitStore(result.mutPtr(), lowerExpression(arm.value))
                        }
                    )
                },
                buildBlock(
                    elseArm.location,
                    elseBlockName
                ) {
                    emitStore(result.mutPtr(), lowerExpression(elseArm.value))
                }
            )
        )

        return result.ptr().load()
    }

    private fun lowerByteCharExpression(expression: Expression.ByteCharLiteral): HIROperand {
        return HIRConstant.IntValue(
            expression.location,
            expression.type,
            expression.value.code
        )
    }

    private fun lowerUnaryMinus(expression: Expression.UnaryMinus): HIRExpression {
        val type = expression.expression.type
        val inner = lowerExpression(expression.expression)
        return when (type) {
            is Type.Integral,
            is Type.FloatingPoint,
            is Type.Size
            -> {
                emit(
                    HIRStatement.BinOp(
                        expression.location,
                        ctx.makeUniqueName(),
                        inner.type,
                        if (type is Type.FloatingPoint) {
                            HIRConstant.FloatValue(
                                expression.location,
                                type,
                                0.0
                            )
                        } else {
                            HIRConstant.IntValue(
                                expression.location,
                                type,
                                0
                            )
                        },
                        BinaryOperator.MINUS,
                        inner
                    )
                ).result()
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerBlockExpression(expression: Expression.BlockExpression): HIRExpression {
        val resultRef = emitAlloca("block_expression_result", expression.type)
        val blockWithoutLastMember = expression.block.copy(members = expression.block.members.dropLast(1))
        val lastMember = expression.block.members.lastOrNull()
        val loweredBlock = lowerBlock(blockWithoutLastMember) {
            val resultVal = when (lastMember) {
                is Block.Member.Expression -> {
                    lowerExpression(lastMember.expression)
                }
                is Block.Member.Statement -> {
                    check(expression.type is Type.Void)
                    lowerStatement(lastMember.statement)
                    HIRConstant.Void(currentLocation)
                }
                null -> {
                    check(expression.type is Type.Void)
                    HIRConstant.Void(currentLocation)
                }
            }
            emitStore(resultRef.mutPtr(), resultVal)
        }
        emit(
            HIRStatement.MatchInt(
                currentLocation,
                HIRConstant.IntValue(currentLocation, Type.u8, 1),
                arms = listOf(
                    MatchIntArm(
                        HIRConstant.IntValue(currentLocation, Type.u8, 1),
                        loweredBlock
                    )
                ),
                otherwise = buildBlock {}
            )
        )
        return resultRef.ptr().load()
    }

    private fun postLowerExpression(expression: HIRExpression): HIRExpression {
        return expression
    }

    private fun lowerAsExpression(expression: Expression.As): HIROperand {
        val type = expression.type
        require(type is Type.Integral || type is Type.Size)
        return emitIntegerConvert(lowerExpression(expression.lhs).asOperand(), type)
    }

    override fun HIRExpression.asOperand(): HIROperand {
        return this
    }

    private fun lowerTypeApplication(expression: Expression.TypeApplication): HIRExpression {
        return lowerExpression(expression.lhs)
    }

    private fun thisParamType(): Type {
        val functionDef = requireNotNull(currentFunctionDef)
        val extensionDef = requireNotNull(currentExtensionDef)
        val extensionForType = lowerTypeAnnotation(extensionDef.forType)
        val thisParamFlags = requireNotNull(functionDef.signature.thisParamFlags)
        return if (thisParamFlags.isPointer) {
            Type.Ptr(extensionForType, isMutable = thisParamFlags.isMutable)
        } else {
            extensionForType
        }
    }

    private fun lowerThisExpression(expression: Expression.This): HIROperand {
        val fn = ctx.resolver.getEnclosingFunction(expression)
        checkNotNull(fn)
        checkNotNull(fn.signature.thisParamBinder)
        return HIRExpression.ParamRef(
            expression.location,
            thisParamType(),
            name = ctx.makeName("this"),
            binder = fn.signature.thisParamBinder,
        )
    }

    private fun lowerPointerCast(expression: Expression.PointerCast): HIROperand {
        val value = lowerExpression(expression.arg)
        require(value.type is Type.Ptr)
        val type = lowerTypeAnnotation(expression.toType)
        return value.ptrCast(type)
    }

    private fun lowerDerefExpression(expression: Expression.Deref): HIROperand {
        return when (ctx.analyzer.typeOfExpression(expression.expression)) {
            is Type.Ptr -> {
                lowerExpression(expression.expression).load()
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerAddressOfMut(expression: Expression.AddressOfMut): HIROperand {
        return when (val expr = expression.expression) {
            is Expression.Var -> {
                val binding = ctx.resolver.resolve(expr.name)
                require(binding is Binding.ValBinding)
                HIRExpression.LocalRef(
                    expression.location,
                    expression.type as Type.Ptr,
                    lowerLocalBinder(binding.statement.binder)
                )
            }
            is Expression.Property -> addressOfStructField(expr)
            else -> requireUnreachable()
        }
    }
    private fun addressOfStructField(expr: Expression.Property): HIROperand {
        return when (val propertyBinding = ctx.analyzer.resolvePropertyBinding(expr)) {
            is PropertyBinding.StructPointerFieldLoad -> {
                lowerExpression(expr.lhs)
                    .fieldPtr(propertyBinding.member.binder.name)
            }
            is PropertyBinding.StructField -> {
                require(expr.lhs is Expression.Var)
                val lhsBinding = ctx.resolver.resolve(expr.lhs.name)
                require(lhsBinding is Binding.ValBinding)
                val structPtr = HIRExpression.LocalRef(
                    expr.location,
                    expr.lhs.type.mutPtr(),
                    lowerLocalBinder(lhsBinding.statement.binder)
                )
                structPtr.fieldPtr(propertyBinding.member.binder.name)
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerAddressOfExpression(expression: Expression.AddressOf): HIROperand {
        return when (val expr = expression.expression) {
            is Expression.Var -> {
                val binding = ctx.resolver.resolve(expr.name)
                require(binding is Binding.ValBinding)
                HIRExpression.LocalRef(
                    expression.location,
                    expression.type as Type.Ptr,
                    lowerLocalBinder(binding.statement.binder)
                )
            }
            is Expression.Property -> {
                addressOfStructField(expr)
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerSizeOfExpression(expression: Expression.SizeOf): HIROperand {
        return HIRConstant.SizeOf(
            expression.location,
            typeOfExpression(expression),
            lowerTypeAnnotation(expression.type)
        )
    }

    private fun lowerIfExpression(expression: Expression.If): HIROperand {
        val resultRef = emitAlloca("if_result", typeOfExpression(expression))
        val trueBlock = buildBlock(expression.trueBranch.location) {
            emitStore(resultRef.mutPtr(), lowerExpression(expression.trueBranch))
        }
        val falseBlock = buildBlock(expression.falseBranch.location) {
            emitStore(resultRef.mutPtr(), lowerExpression(expression.falseBranch))
        }
        emit(
            ifStatement(
                expression.location,
                lowerExpression(expression.condition),
                trueBlock,
                falseBlock
            )
        )
        return resultRef.ptr().load()
    }

    private fun lowerNullPtr(expression: Expression.NullPtr): HIROperand {
        return HIRConstant.NullPtr(expression.location, typeOfExpression(expression) as Type.Ptr)
    }

    private fun lowerBinaryExpression(expression: Expression.BinaryOperation): HIRExpression {
        return when (expression.operator) {
            BinaryOperator.AND -> lowerAndExpression(expression)
            BinaryOperator.OR -> lowerOrExpression(expression)
            else -> emit(
                HIRStatement.BinOp(
                    expression.location,
                    ctx.makeUniqueName(),
                    typeOfExpression(expression),
                    lowerExpression(expression.lhs),
                    expression.operator,
                    lowerExpression(expression.rhs)
                )
            ).result()
        }
    }

    private fun lowerOrExpression(expression: Expression.BinaryOperation): HIROperand {
        val resultRef = allocaAssign(ctx.makeUniqueName(), lowerExpression(expression.lhs))
        emit(
            ifStatement(
                currentLocation,
                resultRef.ptr().load(),
                trueBranch = buildBlock {},
                falseBranch = buildBlock {
                    emitStore(resultRef.mutPtr(), lowerExpression(expression.rhs))
                }
            )
        )
        return resultRef.ptr().load()
    }

    private fun lowerAndExpression(expression: Expression.BinaryOperation): HIROperand {
        val resultRef = allocaAssign(ctx.makeUniqueName(), lowerExpression(expression.lhs))
        emit(
            ifStatement(
                currentLocation,
                resultRef.ptr().load(),
                trueBranch = buildBlock {
                    emitStore(resultRef.mutPtr(), lowerExpression(expression.rhs))
                },
                falseBranch = buildBlock {}
            )
        )

        return resultRef.ptr().load()
    }

    private fun lowerNotExpression(expression: Expression.Not): HIROperand {
        val s = emit(HIRStatement.Not(ctx.makeUniqueName(), lowerExpression(expression.expression)))
        return HIRExpression.LocalRef(s.location, Type.Bool, s.name)
    }

    private fun lowerIntLiteral(expression: Expression.IntLiteral): HIROperand {
        val exprType = ctx.analyzer.reduceGenericInstances(expression.type)
        return if (exprType.isIntegral()) {
            HIRConstant.IntValue(
                expression.location,
                typeOfExpression(expression),
                expression.value
            )
        } else {
            check(exprType is Type.FloatingPoint)
            HIRConstant.FloatValue(
                expression.location,
                exprType,
                expression.value.toDouble()
            )
        }
    }

    private fun lowerBoolLiteral(expression: Expression.BoolLiteral): HIROperand {
        return HIRConstant.BoolValue(
            expression.location,
            typeOfExpression(expression),
            expression.value
        )
    }

    private fun lowerPropertyExpression(expression: Expression.Property): HIROperand = when (val binding = ctx.analyzer.resolvePropertyBinding(expression)) {
        null -> requireUnreachable()
        is PropertyBinding.Global -> exprGen.lowerBinding(expression, binding.binding)
        is PropertyBinding.StructField -> lowerStructFieldBinding(expression)
        is PropertyBinding.StructPointerFieldLoad -> lowerStructPointerFieldLoad(expression)
        is PropertyBinding.ExtensionDef -> lowerExtensionPropertyBinding(expression, binding)
        is PropertyBinding.WhereParamRef -> TODO()
        is PropertyBinding.EnumTypeCaseConstructor -> lowerEnumCaseConstructor(expression, binding)
        is PropertyBinding.WhenCaseFieldRef -> lowerWhenCaseFieldRef(expression, binding)
        is PropertyBinding.TraitFunctionRef -> traitGen.lowerTraitFunctionRef(expression, binding)
    }

    private fun lowerExtensionPropertyBinding(expression: Expression.Property, binding: PropertyBinding.ExtensionDef): HIROperand {
        return HIRExpression.GlobalRef(
            expression.location,
            binding.type,
            extensionMethodName(binding.extensionDef, binding.functionDef)
        )
    }

    private fun lowerWhenCaseFieldRef(expression: Expression.Property, binding: PropertyBinding.WhenCaseFieldRef): HIROperand {
        return HIRExpression.LocalRef(
            expression.lhs.location,
            caseType(binding),
            binding.name.name
        )
            .load()
            .getStructField(binding.propertyName.name)
    }

    private fun caseType(binding: PropertyBinding.WhenCaseFieldRef): Type {
        val constructorType = Type.Constructor(
            name = ctx.resolver.qualifiedName(binding.declaration.name).append(binding.caseName)
        )
        return if (binding.typeArgs.isEmpty()) {
            constructorType
        } else {
            Type.Application(
                constructorType,
                binding.typeArgs
            )
        }
    }

    private fun lowerEnumCaseConstructor(expression: Expression.Property, binding: PropertyBinding.EnumTypeCaseConstructor): HIROperand {
        require(expression.lhs is Expression.Var)
        val name = ctx.resolver.qualifiedName(binding.declaration.name).append(binding.case.name.identifier.name).append(ctx.makeName("constructor"))

        val originalTy = lowerType(typeOfExpression(expression))
        // if this is a case constructor without parameters,
        // its type is actually a parameterless function that
        // returns the instance, instead of the instance itself
        // in lowerExpression, when the actual type arguments are
        // resolved, an implicit call to this function is added
        val ty =
            if (binding.case.params == null) {
                if (originalTy is Type.TypeFunction) {
                    originalTy.copy(
                        body = Type.FunctionPtr(
                            listOf(),
                            originalTy.body
                        )
                    )
                } else {
                    Type.FunctionPtr(
                        listOf(),
                        originalTy
                    )
                }
            } else {
                originalTy
            }
        return HIRExpression.GlobalRef(
            expression.location,
            ty,
            name
        )
    }

    private fun lowerStructPointerFieldLoad(
        expression: Expression.Property
    ): HIROperand {
        val structPtrType = expression.lhs.type
        require(structPtrType is Type.Ptr)
        return lowerExpression(expression.lhs)
            .fieldPtr(expression.property.name)
            .load()
    }

    private fun lowerStructFieldBinding(
        expression: Expression.Property
    ): HIROperand {
        if (ctx.analyzer.isRefStructType(expression.lhs.type)) {
            return lowerExpression(expression.lhs)
                .loadRefField(expression.property.name)
        }
        return lowerExpression(expression.lhs)
            .getStructField(expression.property.name)
    }

    private val externDefs = mutableMapOf<QualifiedName, HIRDefinition.ExternFunction>()
    override fun getExternDef(declaration: Declaration.ExternFunctionDef) = externDefs.computeIfAbsent(lowerGlobalName(declaration.binder)) {
        HIRDefinition.ExternFunction(
            declaration.location,
            name = lowerGlobalName(declaration.binder),
            params = declaration.paramTypes.map { lowerTypeAnnotation(it) },
            externName = declaration.externName.name,
            returnType = lowerTypeAnnotation(declaration.returnType)
        )
    }

    private fun lowerByteString(expression: Expression.ByteString): HIROperand {
        return HIRConstant.ByteString(
            expression.location,
            typeOfExpression(expression),
            expression.bytes
        )
    }

    private val typeOfExpressionCache = mutableMapOf<SourceLocation, Type>()
    override fun typeOfExpression(expression: Expression): Type = typeOfExpressionCache.getOrPut(expression.location) {
        return ctx.analyzer.reduceGenericInstances(ctx.analyzer.typeOfExpression(expression))
    }

    private fun checkUninferredGenerics(node: HasLocation, type: Type) {
        var genericInstanceFound: Type.GenericInstance? = null
        object : TypeTransformer {
            override fun lowerGenericInstance(type: Type.GenericInstance): Type {
                genericInstanceFound = type
                return super.lowerGenericInstance(type)
            }
        }.lowerType(type)

        when (val instance = genericInstanceFound) {
            null -> {}
            else -> ctx.diagnosticReporter.report(node.location, Diagnostic.Kind.UninferrableTypeParam(instance.originalName, instance.location))
        }
    }

    override fun lowerTypeAnnotation(annotation: TypeAnnotation): Type {
        return lowerType(ctx.analyzer.reduceGenericInstances(ctx.analyzer.annotationToType(annotation)))
    }

    private fun lowerParam(param: Param): HIRParam {
        return HIRParam(
            param.location,
            binder = param.binder,
            type = lowerType(ctx.analyzer.typeOfBinder(param.binder))
        )
    }

    override fun lowerTypeParam(typeParam: TypeParam): HIRTypeParam {
        return HIRTypeParam(
            location = typeParam.location,
            name = typeParam.binder.identifier.name,
            id = typeParam.binder.id,
        )
    }

    override fun lowerGlobalName(binder: Binder): QualifiedName {
        val binding = requireNotNull(ctx.resolver.resolve(binder.identifier))
        if (binding is Binding.GlobalFunction && binding.declaration.externName != null) {
            return QualifiedName(listOf(binding.declaration.externName.name))
        }
        return ctx.resolver.resolveGlobalName(binder)
    }
}

internal fun hasTerminator(body: Block): Boolean {
    if (body.members.isEmpty()) {
        return false
    }
    val last = body.members.last()

    return last is Block.Member.Statement && last.statement is Statement.Return
}
