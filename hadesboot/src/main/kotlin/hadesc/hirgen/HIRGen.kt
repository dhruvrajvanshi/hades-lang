package hadesc.hirgen

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.context.NamingContext
import hadesc.defer
import hadesc.scoped
import hadesc.diagnostics.Diagnostic
import hadesc.frontend.PropertyBinding
import hadesc.hir.HIRStatement.Companion.ifStatement
import hadesc.hir.transformers.ParamToLocal
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.types.toSubstitution
import libhades.collections.Stack
import hadesc.hir.*
import hadesc.ignore
import hadesc.types.mutPtr

internal typealias ValueSubstitution =
        MutableMap<Binder, HIRBuilder.() -> HIROperand>

internal interface HIRGenModuleContext {
    val enumTagFieldName: Name
    val currentModule: HIRModule
    var valueSubstitution: ValueSubstitution
    var localAssignmentSubstitution: ValueSubstitution
    fun lowerGlobalName(binder: Binder): QualifiedName
    fun typeOfExpression(expression: Expression): Type
    fun lowerLocalBinder(binder: Binder): Name

    /**
     * FIXME: It would be a better design to take binder name as the parameter so that
     *        callers don't have to know about ExternFunctionDef.
     */
    fun getExternDef(declaration: Declaration.ExternFunctionDef): HIRDefinition.ExternFunction
}

internal interface HIRGenFunctionContext: HIRBuilder {
    val scopeStack: HIRGenScopeStack
    val paramToLocal: ParamToLocal
    fun lowerExpression(expression: Expression): HIRExpression
    fun lowerBlock(
        body: Block,
        addReturnVoid: Boolean = false,
        header: List<HIRStatement> = emptyList(),
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
class HIRGen(private val ctx: Context): ASTContext by ctx, HIRGenModuleContext, HIRGenFunctionContext, NamingContext by ctx, HIRBuilder {
    override val namingCtx: NamingContext get() = ctx
    override val paramToLocal = ParamToLocal(ctx)
    override val enumTagFieldName = ctx.makeName("\$tag")
    override val currentModule = HIRModule(mutableListOf())
    override var valueSubstitution: ValueSubstitution = mutableMapOf()
    override var localAssignmentSubstitution: ValueSubstitution = mutableMapOf()

    private val exprGen = HIRGenExpression(
        ctx,
        moduleContext = this,
        functionContext = this
    )
    private val closureGen = HIRGenClosure(
        ctx,
        moduleContext = this,
        functionContext = this
    )
    override lateinit var currentLocation: SourceLocation
    override var currentStatements: MutableList<HIRStatement>? = null
    override val scopeStack = HIRGenScopeStack()

    fun lowerSourceFiles(sourceFiles: Collection<SourceFile>): HIRModule {
        val declarations = currentModule.definitions
        try {
            for (sourceFile in sourceFiles) scoped {
                currentLocation = sourceFile.location
                scopeStack.push(sourceFile)
                defer { check(scopeStack.pop() === sourceFile) }
                for (it in sourceFile.declarations) {
                    declarations.addAll(lowerDeclaration(it))
                }
            }
            declarations.addAll(externDefs.values)
            val result = HIRModule(declarations)
            logger().debug("HIRGen output")
            logger().debug(result.prettyPrint())
            return result
        } catch (e: Error) {
            logger().error("Error while compiling module", e)
            logger().debug("Generated sources:\n${declarations.joinToString("\n\n") { it.prettyPrint() }}")
            logger().debug("current statements:\n${currentStatements?.joinToString("\n") { it.prettyPrint() }}")
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
        is Declaration.TraitDef -> emptyList()
        is Declaration.ImplementationDef -> lowerImplementationDef(declaration)
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

    private fun lowerImplementationDef(declaration: Declaration.ImplementationDef): List<HIRDefinition> {
        val traitDecl = ctx.resolver.resolveDeclaration(declaration.traitRef)
        require(traitDecl is Declaration.TraitDef)
        return listOf(
                HIRDefinition.Implementation(
                        declaration.location,
                        typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                        traitName = ctx.resolver.qualifiedName(traitDecl.name),
                        traitArgs = declaration.traitArguments.map { lowerTypeAnnotation(it) },
                        functions = declaration.body.filterIsInstance<Declaration.FunctionDef>().map {
                            lowerFunctionDef(it, QualifiedName(listOf(it.name.identifier.name)))
                        },
                        typeAliases = declaration.body.filterIsInstance<Declaration.TypeAlias>().associate {
                            require(it.typeParams == null)
                            it.name.name to lowerTypeAnnotation(it.rhs)
                        },
                        traitRequirements = declaration.whereClause?.traitRequirements?.map {
                            lowerTraitRequirement(it)
                        } ?: emptyList()
                )
        )
    }

    private fun lowerTraitRequirement(requirement: TraitRequirementAnnotation): TraitRequirement {
        val traitDef = ctx.resolver.resolveDeclaration(requirement.path)
        require(traitDef is Declaration.TraitDef)
        return TraitRequirement(
            ctx.resolver.qualifiedName(traitDef.name),
            requirement.typeArgs?.map { lowerTypeAnnotation(it) } ?: emptyList()
        )
    }

    private var currentExtensionDef: Declaration.ExtensionDef? = null
    private fun lowerExtensionDef(declaration: Declaration.ExtensionDef): List<HIRDefinition> {
        require(currentExtensionDef == null)
        currentExtensionDef = declaration
        val list = mutableListOf<HIRDefinition>()
        for (functionDef in declaration.functionDefs) {
            list.add(lowerFunctionDef(
                    functionDef,
                    extensionMethodName(declaration, functionDef)))
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
        val caseStructs = declaration.cases.flatMapIndexed { index, case -> listOf(
            HIRDefinition.Struct(
                case.name.location,
                enumName.append(case.name.identifier.name),
                typeParams = declaration.typeParams?.map { HIRTypeParam(it.location, it.binder.identifier.name) },
                fields = (case.params?.mapIndexed { caseParamIndex, it ->
                    ctx.makeName("$caseParamIndex") to ctx.analyzer.annotationToType(requireNotNull(it.annotation))
                } ?: emptyList())
            ),
            HIRDefinition.Const(
                case.name.location,
                caseTagName(enumName, case),
                HIRConstant.IntValue(
                    case.name.location,
                    ctx.enumTagType(),
                    index
                )
            ),
            sealedCaseConstructorOrConst(declaration, enumName, case)
        )}.toTypedArray()
        return listOf(
            *caseStructs,
            HIRDefinition.Struct(
                declaration.location,
                lowerGlobalName(declaration.name),
                typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                fields = listOf(
                    enumTagFieldName to ctx.enumTagType(),
                    ctx.makeName("payload") to ctx.analyzer.getEnumPayloadType(declaration)
                )
            )
        )
    }

    private fun caseTagName(enumName: QualifiedName, case: Declaration.Enum.Case): QualifiedName {
        return enumName.append(case.name.identifier.name).append(ctx.makeName("tag"))
    }

    private fun enumInstanceType(declaration: Declaration.Enum): Type {
        val typeConstructor = Type.Constructor(name = ctx.resolver.qualifiedName(declaration.name))
        return if (declaration.typeParams != null) {
            Type.Application(
                typeConstructor,
                declaration.typeParams.map { Type.ParamRef(it.binder) }
            )
        } else {
            typeConstructor
        }
    }

    private fun sealedCaseConstructorOrConst(
        declaration: Declaration.Enum,
        enumName: QualifiedName,
        case: Declaration.Enum.Case,
    ): HIRDefinition.Function {
        val caseStructName = enumName.append(case.name.name)
        val fnName = caseStructName.append(ctx.makeName("constructor"))
        val enumStructRefType = typeOfEnumConstructor(declaration)
        val enumStructRef = HIRExpression.GlobalRef(case.name.location, enumStructRefType, enumName)
        val caseStructRefType = enumCaseConstructorRefType(declaration, case)
        val caseStructRef = HIRExpression.GlobalRef(case.name.location, caseStructRefType, caseStructName)
        val payloadTypeUnion = ctx.analyzer.getEnumPayloadType(declaration)
        val baseInstanceType = typeOfEnumInstance(declaration, declaration.typeParams?.map { Type.ParamRef(it.binder) })

        val loc = case.name.location
        val tag = HIRExpression.GlobalRef(loc, ctx.enumTagType(), caseTagName(enumName, case))
        val body = buildBlock(case.name.location, ctx.makeName("entry")) {
            val payloadRef = emitAlloca("payload", payloadTypeUnion)
            val caseFn: HIROperand =
                if (caseStructRefType !is Type.TypeFunction)
                    caseStructRef
                else
                    emitTypeApplication(
                        caseStructRef,
                        declaration.typeParams?.map { Type.ParamRef(it.binder) } ?: emptyList()
                    ).result()
            if (case.params != null && case.params.isNotEmpty()) {
                emitStore(
                    payloadRef.mutPtr(),
                    emitCall(
                        payloadRef.type,
                        caseFn,
                        case.params.mapIndexed { index, it ->
                            HIRExpression.ParamRef(it.annotation.location, lowerTypeAnnotation(checkNotNull(it.annotation)), ctx.makeName("param_$index"), Binder(Identifier(it.annotation.location, ctx.makeName("param_$index"))))
                        }
                    ).result()
                )
            }
            val baseStructRefApplied =
                if (declaration.typeParams == null)
                    enumStructRef
                else {
                    check(enumStructRefType is Type.TypeFunction)
                    check(enumStructRefType.params.size == declaration.typeParams.size)
                    emitTypeApplication(
                        enumStructRef,
                        declaration.typeParams.map { Type.ParamRef(it.binder) }
                    ).result()
                }
            val baseConstructorCall = emitCall(
                baseInstanceType,
                baseStructRefApplied,
                listOf(
                    tag,
                    payloadRef.ptr().load()
                )
            ).result()
            emit(
                HIRStatement.Return(
                    case.name.location,
                    baseConstructorCall
                )
            )
        }

        return HIRDefinition.Function(
            case.name.location,
            HIRFunctionSignature(
                case.name.location,
                fnName,
                declaration.typeParams?.map { HIRTypeParam(it.location, it.binder.name) },
                params = case.params?.mapIndexed { index, param ->
                    HIRParam(
                        param.annotation.location,
                        Binder(Identifier(param.annotation.location, ctx.makeName("param_$index"))),
                        lowerTypeAnnotation(checkNotNull(param.annotation))) } ?: emptyList(),
                returnType = baseInstanceType
            ),
            mutableListOf(body)
        )
    }

    private fun typeOfEnumConstructor(declaration: Declaration.Enum): Type {
        val instanceType = typeOfEnumInstance(declaration, declaration.typeParams?.map { Type.ParamRef(it.binder) })
        val paramTypes = listOf(
            ctx.enumTagType(),
            ctx.analyzer.getEnumPayloadType(declaration)
        )

        val fnType = Type.Function(
            from = paramTypes,
            to = instanceType,
            traitRequirements = null
        )

        val fnPtrType = Type.Ptr(fnType, isMutable = false)

        return if (declaration.typeParams != null) {
            Type.TypeFunction(
                params = declaration.typeParams.map { Type.Param(it.binder) },
                body = fnPtrType
            )
        } else {
            fnPtrType
        }
    }

    private fun typeOfEnumInstance(declaration: Declaration.Enum, typeArgs: List<Type>?): Type {
        val instanceTypeConstructor = Type.Constructor(lowerGlobalName(declaration.name))
        return if (typeArgs != null) {
            Type.Application(instanceTypeConstructor, typeArgs)
        } else {
            instanceTypeConstructor
        }
    }

    private fun enumCaseConstructorRefType(
        declaration: Declaration.Enum,
        case: Declaration.Enum.Case
    ): Type {
        val instanceType = enumInstanceType(declaration)
        val from = case.params?.map { lowerTypeAnnotation(checkNotNull(it.annotation)) } ?: emptyList()
        val functionType = Type.Function(
            from,
            instanceType
        )

        return if (declaration.typeParams != null) {
            Type.TypeFunction(
                declaration.typeParams.map { Type.Param(it.binder) },
                functionType
            )
        } else functionType
    }

    private var currentFunctionDef: Declaration.FunctionDef? = null
    private fun lowerFunctionDef(
            declaration: Declaration.FunctionDef,
            qualifiedName: QualifiedName? = null
    ): HIRDefinition.Function = scoped {
        check(currentFunctionDef == null)
        currentFunctionDef = declaration
        defer {
            check(currentFunctionDef === declaration)
            currentFunctionDef = null
        }

        scopeStack.push(declaration)
        defer { check(scopeStack.pop() === declaration) }

        val returnType = lowerTypeAnnotation(declaration.signature.returnType)
        val addReturnVoid = returnType is Type.Void && !hasTerminator(declaration.body)
        val signature = lowerFunctionSignature(declaration.signature, qualifiedName)
        val header = paramToLocal.declareParamCopies(signature.params)
        val body = lowerBlock(
            declaration.body,
            addReturnVoid,
            header,
        ).copy(name = ctx.makeName("entry"))
        HIRDefinition.Function(
                location = declaration.location,
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
            params.add(HIRParam(
                    signature.location,
                    binder = checkNotNull(signature.thisParamBinder),
                    type = thisParamType()
            ))
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

    private fun hasTerminator(body: Block): Boolean {
        if (body.members.isEmpty()) {
            return false
        }
        val last = body.members.last()

        return last is Block.Member.Statement && last.statement is Statement.Return
    }

    private val deferStack = Stack<MutableList<HIRStatement>>()
    override fun lowerBlock(
        body: Block,
        addReturnVoid: Boolean,
        header: List<HIRStatement>,
        before: HIRBuilder.() -> Unit,
        after: HIRBuilder.() -> Unit,
    ): HIRBlock = scoped {
        scopeStack.push(body)
        defer { check(scopeStack.pop() === body) }
        buildBlock(body.location) {
            deferStack.push(mutableListOf())
            before()
            header.forEach { emit(it) }
            for (member in body.members) {
                lowerBlockMember(member)
            }
            after()
            if (addReturnVoid) {
                terminateScope()
                emit(HIRStatement.Return(body.location, HIRConstant.Void(body.location)))
            } else {
                for (statement in requireNotNull(deferStack.peek())) {
                    emit(statement)
                }
            }
            deferStack.pop()
        }
    }

    private fun lowerBlockMember(member: Block.Member): Unit = when(member) {
        is Block.Member.Expression ->
            allocaAssign(
                ctx.makeUniqueName(),
                lowerExpression(member.expression)
            ).ignore()
        is Block.Member.Statement -> lowerStatement(member.statement)
    }

    private fun lowerStatement(statement: Statement): Unit = when(statement) {
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
        intoStatementList(requireNotNull(deferStack.peek())) {
            lowerBlockMember(statement.blockMember)
        }
    }

    private fun lowerMemberAssignmentStatement(statement: Statement.MemberAssignment) {
        require(statement.lhs.lhs is Expression.Var)
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

    private fun lowerPointerAssignment(statement: Statement.PointerAssignment) {
        val ptr = lowerExpression(statement.lhs.expression)
        check(ptr is HIROperand)
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
        val substitutionPtr = localAssignmentSubstitution[binding.binder]
        if (substitutionPtr != null) {
            emitStore(substitutionPtr(), lowerExpression(statement.value))
        } else {
            emitAssign(lowerLocalBinder(binding.statement.binder), lowerExpression(statement.value))
        }
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

    private fun lowerValStatement(statement: Statement.Val) {
        val name = lowerLocalBinder(statement.binder)
        allocaAssign(name, lowerExpression(statement.rhs))
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
            emit(HIRStatement.Return(statement.location, HIRConstant.Void(statement.location)))
        } else {
            emit(
                HIRStatement.Return(
                    statement.location,
                    lowerExpression(statement.value)
                )
            )
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
            is Expression.Closure -> closureGen.lowerClosure(expression)
            is Expression.As -> lowerAsExpression(expression)
            is Expression.BlockExpression -> lowerBlockExpression(expression)
            is Expression.Intrinsic -> requireUnreachable()
            is Expression.UnaryMinus -> lowerUnaryMinus(expression)
            is Expression.ByteCharLiteral -> lowerByteCharExpression(expression)
            is Expression.Match -> lowerMatchExpression(expression)
            is Expression.FloatLiteral -> lowerFloatLiteral(expression)
        }
        val typeArgs = ctx.analyzer.getTypeArgs(expression)
        val exprType = lowered.type
        checkUninferredGenerics(expression, exprType)
        val withAppliedTypes = if (typeArgs != null) {
            check(exprType is Type.TypeFunction)
            check(exprType.params.size == typeArgs.size)
            check(lowered is HIROperand)
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
            check(withAppliedTypes is HIROperand)
            if (enumConstructorBinding.case.params == null) {
                emitCall(
                    withAppliedTypes.type,
                    withAppliedTypes,
                    emptyList(),
                ).result()
            } else {
                withAppliedTypes
            }
        } else withAppliedTypes
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
            is Type.Size,
            -> {
                emit(HIRStatement.BinOp(
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
                )).result()
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
        emit(HIRStatement.MatchInt(
            currentLocation,
            HIRConstant.IntValue(currentLocation, Type.u8, 1),
            arms = listOf(
                MatchIntArm(
                    HIRConstant.IntValue(currentLocation, Type.u8, 1),
                    loweredBlock
                )
            ),
            otherwise = buildBlock {}
        ))
        return resultRef.ptr().load()
    }

    private fun postLowerExpression(expression: HIRExpression): HIRExpression {
        return when (expression) {
            is HIRExpression.ParamRef -> paramToLocal.fixParamRef(expression)
            else -> expression
        }
    }

    private fun lowerAsExpression(expression: Expression.As): HIROperand {
        val type = expression.type
        require(type is Type.Integral || type is Type.Size)

        return emitIntegerConvert(lowerExpression(expression.lhs), type)
    }

    private fun lowerTypeApplication(expression: Expression.TypeApplication): HIRExpression {
        return lowerExpression(expression.lhs)
    }

    private fun thisParamType(): Type {
        val functionDef = requireNotNull(currentFunctionDef)
        val extensionDef = requireNotNull(currentExtensionDef)
        val extensionForType = lowerTypeAnnotation(extensionDef.forType)
        val thisParamFlags = requireNotNull(functionDef.signature.thisParamFlags)
        return if (thisParamFlags.isPointer)
            Type.Ptr(extensionForType, isMutable = thisParamFlags.isMutable)
        else extensionForType
    }

    private fun lowerThisExpression(expression: Expression.This): HIROperand {
        return HIRExpression.ParamRef(
                expression.location,
                thisParamType(),
                name = ctx.makeName("this"),
                binder = Binder(Identifier(expression.location, ctx.makeName("this")))
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
                    .fieldPtr(
                        propertyBinding.member.binder.name,
                        propertyBinding.memberIndex,
                        expr.type.mutPtr())
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
                structPtr.fieldPtr(
                    propertyBinding.member.binder.name,
                    propertyBinding.memberIndex,
                    expr.type.mutPtr()
                )
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
                lowerTypeAnnotation(expression.type))
    }

    private fun lowerIfExpression(expression: Expression.If): HIROperand {
        val resultRef = emitAlloca("if_result", typeOfExpression(expression))
        val trueBlock = buildBlock(expression.trueBranch.location) {
            emitStore(resultRef.mutPtr(), lowerExpression(expression.trueBranch))
        }
        val falseBlock = buildBlock(expression.falseBranch.location) {
            emitStore(resultRef.mutPtr(), lowerExpression(expression.falseBranch))
        }
        emit(ifStatement(
                expression.location,
                lowerExpression(expression.condition),
                trueBlock,
                falseBlock
        ))
        return resultRef.ptr().load()
    }

    private fun lowerNullPtr(expression: Expression.NullPtr): HIROperand {
        return HIRConstant.NullPtr(expression.location, typeOfExpression(expression) as Type.Ptr)
    }

    private fun lowerBinaryExpression(expression: Expression.BinaryOperation): HIRExpression {
        return when (expression.operator) {
            BinaryOperator.AND -> lowerAndExpression(expression)
            BinaryOperator.OR  -> lowerOrExpression(expression)
            else -> emit(HIRStatement.BinOp(
                expression.location,
                ctx.makeUniqueName(),
                typeOfExpression(expression),
                lowerExpression(expression.lhs),
                expression.operator,
                lowerExpression(expression.rhs)
            )).result()
        }
    }

    private fun lowerOrExpression(expression: Expression.BinaryOperation): HIROperand {
        val resultRef = allocaAssign(ctx.makeUniqueName(), lowerExpression(expression.lhs))
        emit(ifStatement(
            currentLocation,
            resultRef.ptr().load(),
            trueBranch = buildBlock {},
            falseBranch = buildBlock {
                emitStore(resultRef.mutPtr(), lowerExpression(expression.rhs))
            }
        ))
        return resultRef.ptr().load()
    }

    private fun lowerAndExpression(expression: Expression.BinaryOperation): HIROperand {
        val resultRef = allocaAssign(ctx.makeUniqueName(), lowerExpression(expression.lhs))
        emit(ifStatement(
            currentLocation,
            resultRef.ptr().load(),
            trueBranch = buildBlock {
                emitStore(resultRef.mutPtr(), lowerExpression(expression.rhs))
            },
            falseBranch = buildBlock {}
        ))

        return resultRef.ptr().load()
    }

    private fun lowerNotExpression(expression: Expression.Not): HIROperand {
        val s = emit(HIRStatement.Not(ctx.makeUniqueName(), lowerExpression(expression.expression)))
        return HIRExpression.LocalRef(s.location, Type.Bool, s.name)
    }

    private fun lowerIntLiteral(expression: Expression.IntLiteral): HIROperand {
        if (expression.type.isIntegral()) {
            return HIRConstant.IntValue(
                expression.location,
                typeOfExpression(expression),
                expression.value
            )
        } else {
            val exprType = expression.type
            check(exprType is Type.FloatingPoint)
            return HIRConstant.FloatValue(
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

    private fun lowerPropertyExpression(expression: Expression.Property): HIROperand = when(val binding = ctx.analyzer.resolvePropertyBinding(expression)) {
        null -> requireUnreachable()
        is PropertyBinding.Global -> exprGen.lowerBinding(expression, binding.binding)
        is PropertyBinding.StructField -> lowerStructFieldBinding(expression, binding)
        is PropertyBinding.StructPointerFieldLoad -> lowerStructPointerFieldLoad(expression, binding)
        is PropertyBinding.ExtensionDef -> lowerExtensionPropertyBinding(expression, binding)
        is PropertyBinding.WhereParamRef -> TODO()
        is PropertyBinding.EnumTypeCaseConstructor -> lowerEnumCaseConstructor(expression, binding)
        is PropertyBinding.WhenCaseFieldRef -> lowerWhenCaseFieldRef(expression, binding)
        is PropertyBinding.TraitFunctionRef -> lowerTraitFunctionRef(expression, binding)
    }

    private fun lowerTraitFunctionRef(expression: Expression.Property, binding: PropertyBinding.TraitFunctionRef): HIROperand {
        return HIRExpression.TraitMethodRef(
            expression.location,
            expression.type,
            traitName = binding.traitName,
            traitArgs = binding.args,
            methodName = binding.methodName,
        )
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
            binding.name.name,
        )
            .load()
            .getStructField(
                binding.propertyName.name,
                binding.propertyIndex + 1, // 0th field is tag
                typeOfExpression(expression),
            )
    }

    private fun caseType(binding: PropertyBinding.WhenCaseFieldRef): Type {
        val constructorType = Type.Constructor(
            name = ctx.resolver.qualifiedName(binding.declaration.name).append(binding.caseName),
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

        return HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            name
        )
    }

    private fun lowerStructPointerFieldLoad(
        expression: Expression.Property,
        binding: PropertyBinding.StructPointerFieldLoad
    ): HIROperand {
        val structPtrType = expression.lhs.type
        require(structPtrType is Type.Ptr)
        return lowerExpression(expression.lhs)
            .fieldPtr(expression.property.name, binding.memberIndex, structPtrType)
            .load()
    }

    private fun lowerStructFieldBinding(
            expression: Expression.Property,
            binding: PropertyBinding.StructField
    ): HIROperand {
        return lowerExpression(expression.lhs)
            .getStructField(
                expression.property.name,
                binding.memberIndex,
                typeOfExpression(expression)
            )
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


    private fun applyType(type: Type.TypeFunction, args: List<Type>): Type {
        require(type.params.size == args.size)
        return type.body.applySubstitution(
            type.params.zip(args).associate {
                it.first.binder.location to ctx.analyzer.reduceGenericInstances(it.second)
            }.toSubstitution()
        )
    }

    private fun lowerTypeAnnotation(annotation: TypeAnnotation): Type {
        return ctx.analyzer.reduceGenericInstances(ctx.analyzer.annotationToType(annotation))
    }

    private fun lowerParam(param: Param): HIRParam {
        return HIRParam(
                param.location,
                binder = param.binder,
                type = ctx.analyzer.typeOfBinder(param.binder)
        )
    }

    private fun lowerTypeParam(typeParam: TypeParam): HIRTypeParam {
        return HIRTypeParam(location = typeParam.location, name = typeParam.binder.identifier.name)
    }

    override fun lowerGlobalName(binder: Binder): QualifiedName {
        val binding = requireNotNull(ctx.resolver.resolve(binder.identifier))
        if (binding is Binding.GlobalFunction && binding.declaration.externName != null) {
            return QualifiedName(listOf(binding.declaration.externName.name))
        }
        return ctx.resolver.resolveGlobalName(binder)
    }
}