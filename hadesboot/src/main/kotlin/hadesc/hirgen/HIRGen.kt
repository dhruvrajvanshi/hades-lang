package hadesc.hirgen

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.ASTContext
import hadesc.context.Context
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
import hadesc.types.emptySubstitution
import hadesc.types.toSubstitution
import libhades.collections.Stack
import hadesc.hir.*

internal interface HIRGenModuleContext {
    val enumTagFieldName: Name
    fun lowerGlobalName(binder: Binder): QualifiedName
    fun typeOfExpression(expression: Expression): Type
    fun lowerLocalBinder(binder: Binder): Name

    /**
     * FIXME: It would be a better design to take binder name as the parameter so that
     *        callers don't have to know about ExternFunctionDef.
     */
    fun getExternDef(declaration: Declaration.ExternFunctionDef): HIRDefinition.ExternFunction
}

internal interface HIRGenFunctionContext {
    val currentLocation: SourceLocation
    fun lowerExpression(expression: Expression): HIRExpression
    fun emit(statement: HIRStatement)
    fun buildBlock(location: SourceLocation, name: Name? = null, builder: () -> Unit): HIRBlock
    fun declareAndAssign(namePrefix: String = "", rhs: HIRExpression, location: SourceLocation = currentLocation): HIRExpression
    fun declareVariable(namePrefix: String = "", type: Type, location: SourceLocation = currentLocation): HIRExpression.ValRef
}

class HIRGen(private val ctx: Context): ASTContext by ctx, HIRGenModuleContext, HIRGenFunctionContext {
    private val paramToLocal = ParamToLocal(ctx)
    override val enumTagFieldName = ctx.makeName("\$tag")
    private val exprGen = HIRGenExpression(
        ctx,
        moduleContext = this,
        functionContext = this
    )
    override lateinit var currentLocation: SourceLocation

    fun lowerSourceFiles(sourceFiles: Collection<SourceFile>): HIRModule {
        val declarations = mutableListOf<HIRDefinition>()
        try {
            for (sourceFile in sourceFiles) {
                currentLocation = sourceFile.location
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
    ): HIRDefinition {
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
            val payloadVal = declareVariable("payload", payloadTypeUnion)
            val paramSubst = declaration.typeParams?.associate {
                it.location to Type.ParamRef(it.binder)
            }?.toSubstitution() ?: emptySubstitution()
            val caseFn =
                if (caseStructRefType !is Type.TypeFunction)
                    caseStructRef
                else
                    HIRExpression.TypeApplication(
                        loc,
                        caseStructRefType.body.applySubstitution(paramSubst),
                        caseStructRef,
                        declaration.typeParams?.map { Type.ParamRef(it.binder) } ?: emptyList()
                    )
            if (case.params != null && case.params.isNotEmpty()) {
                emit(
                    HIRStatement.Assignment(
                        loc,
                        payloadVal.name,
                        HIRExpression.Call(
                            loc,
                            payloadVal.type,
                            caseFn,
                            case.params.mapIndexed { index, it ->
                                HIRExpression.ParamRef(it.annotation.location, lowerTypeAnnotation(checkNotNull(it.annotation)), ctx.makeName("param_$index"), Binder(Identifier(it.annotation.location, ctx.makeName("param_$index"))))
                            }
                        )
                    )
                )
            }
            val baseStructRefApplied =
                if (declaration.typeParams == null)
                    enumStructRef
                else {
                    check(enumStructRefType is Type.TypeFunction)
                    check(enumStructRefType.params.size == declaration.typeParams.size)
                    HIRExpression.TypeApplication(
                        loc,
                        enumStructRefType.body.applySubstitution(
                            declaration.typeParams.associate {
                                it.location to Type.ParamRef(it.binder)
                            }.toSubstitution()
                        ),
                        enumStructRef,
                        declaration.typeParams.map { Type.ParamRef(it.binder) }
                    )
                }
            val baseConstructorCall = HIRExpression.Call(
                loc,
                baseInstanceType,
                baseStructRefApplied,
                listOf(
                    tag,
                    payloadVal
                )
            )
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
            null,
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
    ): HIRDefinition.Function {
        require(currentFunctionDef == null)
        currentFunctionDef = declaration
        val returnType = lowerTypeAnnotation(declaration.signature.returnType)
        val addReturnVoid = returnType is Type.Void && !hasTerminator(declaration.body)
        val signature = lowerFunctionSignature(declaration.signature, qualifiedName)
        val header = paramToLocal.declareParamCopies(signature.params)
        val body = lowerBlock(
            declaration.body,
            addReturnVoid,
            header,
        ).copy(name = ctx.makeName("entry"))
        currentFunctionDef = null
        return HIRDefinition.Function(
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

    private var currentStatements: MutableList<HIRStatement>? = null
    private val deferStack = Stack<MutableList<HIRStatement>>()
    private fun lowerBlock(
        body: Block,
        addReturnVoid: Boolean = false,
        header: List<HIRStatement> = emptyList()
    ): HIRBlock = buildBlock(body.location) {
        deferStack.push(mutableListOf())
        header.forEach { emit(it) }
        for (member in body.members) {
            lowerBlockMember(member).forEach {
                emit(it)
            }
        }
        if (addReturnVoid) {
            terminateScope()
            emit(HIRStatement.ReturnVoid(body.location))
        } else {
            for (statement in requireNotNull(deferStack.peek())) {
                emit(statement)
            }
        }
        deferStack.pop()
    }

    override fun buildBlock(location: SourceLocation, name: Name?, builder: () -> Unit): HIRBlock {
        val oldStatements = currentStatements
        val statements = mutableListOf<HIRStatement>()
        currentStatements = statements
        builder()
        currentStatements = oldStatements
        return HIRBlock(location, name ?: ctx.makeUniqueName(), statements)
    }

    override fun emit(statement: HIRStatement) {
        requireNotNull(currentStatements).add(statement)
    }

    private fun lowerBlockMember(member: Block.Member): Collection<HIRStatement> = when(member) {
        is Block.Member.Expression -> listOf(HIRStatement.Expression(lowerExpression(member.expression)))
        is Block.Member.Statement -> lowerStatement(member.statement)
    }

    private fun lowerStatement(statement: Statement): Collection<HIRStatement> = when(statement) {
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

    private fun lowerDeferStatement(statement: Statement.Defer): Collection<HIRStatement> {
        requireNotNull(deferStack.peek()).addAll(lowerBlockMember(statement.blockMember).reversed())
        return emptyList()
    }

    private fun lowerMemberAssignmentStatement(statement: Statement.MemberAssignment): Collection<HIRStatement> {
        require(statement.lhs.lhs is Expression.Var)
        val binding = ctx.resolver.resolve(statement.lhs.lhs.name)
        require(binding is Binding.ValBinding)
        require(binding.statement.isMutable)
        val fieldAddr = addressOfStructField(statement.lhs)
        return listOf(
                HIRStatement.Store(
                        location = statement.location,
                        ptr = fieldAddr,
                        value = lowerExpression(statement.value)
                )
        )
    }

    private fun lowerPointerAssignment(statement: Statement.PointerAssignment): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Store(
                        statement.location,
                        ptr = lowerExpression(statement.lhs.expression),
                        value = lowerExpression(statement.value)
                )
        )
    }

    private fun lowerLocalAssignment(statement: Statement.LocalAssignment): Collection<HIRStatement> {
        val binding = ctx.resolver.resolve(statement.name)
        require(binding is Binding.ValBinding)
        return listOf(
                HIRStatement.Assignment(
                        statement.location,
                        name = lowerLocalBinder(binding.statement.binder),
                        value = lowerExpression(statement.value)
                )
        )
    }

    private fun lowerWhileStatement(statement: Statement.While): Collection<HIRStatement> {
        currentLocation = statement.condition.location
        val conditionVar = declareVariable("while_condition", Type.Bool)
        return listOf(
                HIRStatement.While(
                    statement.location,
                    conditionName = conditionVar.name,
                    buildBlock(statement.condition.location, ctx.makeUniqueName("while_condition_block")) {
                       emit(HIRStatement.Assignment(
                           statement.condition.location,
                           conditionVar.name,
                           lowerExpression(statement.condition)
                       ))
                    },
                    lowerBlock(statement.body)
                )
        )
    }

    private fun lowerValStatement(statement: Statement.Val): Collection<HIRStatement> {
        val name = lowerLocalBinder(statement.binder)
        return listOf(
            HIRStatement.ValDeclaration(
                statement.location,
                name,
                statement.isMutable,
                ctx.analyzer.reduceGenericInstances(statement.rhs.type)
            ),
            HIRStatement.Assignment(
                statement.location,
                name,
                lowerExpression(statement.rhs),

            )
        )
    }

    override fun lowerLocalBinder(binder: Binder): Name {
        // TODO: Handle shadowing
        return binder.identifier.name
    }

    private fun lowerIfStatement(statement: Statement.If): Collection<HIRStatement> {
        return listOf(
            ifStatement(
                location = statement.location,
                condition = lowerExpression(statement.condition),
                trueBranch = lowerBlock(statement.ifTrue),
                falseBranch = statement.ifFalse?.let { lowerBlock(it) } ?: HIRBlock(
                        location = statement.location,
                        statements = mutableListOf(),
                        name = ctx.makeUniqueName()
                )
            )
        )
    }

    private fun lowerReturnStatement(statement: Statement.Return): Collection<HIRStatement> {
        terminateScope()
        requireNotNull(deferStack.peek()).removeIf { true }
        return if (statement.value == null) {
            listOf(HIRStatement.ReturnVoid(statement.location))
        } else {
            listOf(
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
            is Expression.Closure -> lowerClosure(expression)
            is Expression.UnsafeCast -> lowerUnsafeCast(expression)
            is Expression.As -> lowerAsExpression(expression)
            is Expression.ArrayIndex -> lowerArrayIndexExpression(expression)
            is Expression.ArrayLiteral -> lowerArrayLiteral(expression)
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
            HIRExpression.TypeApplication(
                expression.location,
                applyType(exprType, typeArgs),
                lowered,
                typeArgs.map { ctx.analyzer.reduceGenericInstances(it) }
            )
        } else {
            check(exprType !is Type.TypeFunction) {
                "${expression.location}"
            }
            postLowerExpression(lowered)
        }

        val enumConstructorBinding = ctx.analyzer.getEnumConstructorBinding(expression)
        return if (enumConstructorBinding != null && expression !is Expression.TypeApplication) {
            if (enumConstructorBinding.case.params == null) {
                HIRExpression.Call(
                    expression.location,
                    withAppliedTypes.type,
                    withAppliedTypes,
                    emptyList(),
                )
            } else {
                withAppliedTypes
            }
        } else withAppliedTypes
    }

    private fun lowerFloatLiteral(expression: Expression.FloatLiteral): HIRExpression {
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


    override fun declareAndAssign(namePrefix: String, rhs: HIRExpression, location: SourceLocation): HIRExpression {
        val variable = declareVariable(namePrefix, rhs.type, location)
        emit(HIRStatement.Assignment(
            location,
            variable.name,
            rhs
        ))

        return variable
    }

    private fun lowerIntegralMatchExpression(expression: Expression.Match): HIRExpression {
        val resultType = typeOfExpression(expression)
        val discriminantType = typeOfExpression(expression.value)
        require(discriminantType.isIntegral())
        val result = declareVariable("result", resultType)
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
                        HIRBlock(arm.location, blockName, mutableListOf(
                            HIRStatement.Assignment(
                                arm.location,
                                result.name,
                                lowerExpression(arm.value)
                            )
                        ))
                    )
                },
                HIRBlock(
                    elseArm.location,
                    elseBlockName,
                    mutableListOf(
                        HIRStatement.Assignment(
                            elseArm.location,
                            result.name,
                            lowerExpression(elseArm.value)
                        )
                    )
                )
            )
        )

        return result
    }

    private fun lowerByteCharExpression(expression: Expression.ByteCharLiteral): HIRExpression {
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
                HIRExpression.BinOp(
                    expression.location,
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
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerBlockExpression(expression: Expression.BlockExpression): HIRExpression {
        val block = lowerBlock(expression.block)
        return HIRExpression.BlockExpression(expression.type, block)
    }

    private fun lowerArrayIndexExpression(expression: Expression.ArrayIndex): HIRExpression {
        val lhs = lowerExpression(expression.lhs)
        val index = lowerExpression(expression.index)
        val lhsType = lhs.type
        check(lhsType is Type.Array)
        return HIRExpression.ArrayIndex(
            expression.location,
            expression.type,
            lhs,
            index
        )
    }

    private fun lowerArrayLiteral(expression: Expression.ArrayLiteral): HIRExpression {
        val exprType = expression.type
        check(exprType is Type.Array)
        val items = expression.items.map {
            val item = lowerExpression(it)
            check(item is HIRConstant)
            item
        }
        return HIRConstant.ArrayLiteral(expression.location, exprType, items)
    }

    private fun postLowerExpression(expression: HIRExpression): HIRExpression {
        return when (expression) {
            is HIRExpression.ParamRef -> paramToLocal.fixParamRef(expression)
            else -> expression
        }
    }

    private fun lowerAsExpression(expression: Expression.As): HIRExpression {
        val type = expression.type
        require(type is Type.Integral || type is Type.Size)
        return HIRExpression.IntegerConvert(
            expression.location,
            type,
            lowerExpression(expression.lhs)
        )
    }

    private fun lowerClosure(expression: Expression.Closure): HIRExpression {
        val params = expression.params.map {
            HIRParam(
                it.location,
                it.binder,
                ctx.analyzer.getParamType(it))
        }
        val header = paramToLocal.declareParamCopies(params)
        val body = when (expression.body) {
            is ClosureBody.Block -> lowerBlock(expression.body.block, header = header)
            is ClosureBody.Expression -> lowerBlock(Block(expression.body.location, null,  listOf(
                Block.Member.Statement(
                    Statement.Return(
                        expression.body.location,
                        expression.body.expression
                    )
                )
            )), header = header)
        }
        val captureData = ctx.analyzer.getClosureCaptures(expression)
        val captures = captureData
            .copy(
                values = captureData.values.mapKeys {
                    paramToLocal.fixBinder(it.key)
                }
            )
        return HIRExpression.Closure(
            expression.location,
            ctx.analyzer.reduceGenericInstances(expression.type),
            captures,
            params,
            ctx.analyzer.getReturnType(expression),
            body
        )
    }

    private fun lowerTypeApplication(expression: Expression.TypeApplication): HIRExpression {
        return lowerExpression(expression.lhs)
    }

    private fun lowerUnsafeCast(expression: Expression.UnsafeCast): HIRExpression {
        return HIRExpression.UnsafeCast(
            location = expression.location,
            type = lowerTypeAnnotation(expression.toType),
            value = lowerExpression(expression.value)
        )
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

    private fun lowerThisExpression(expression: Expression.This): HIRExpression {
        return HIRExpression.ParamRef(
                expression.location,
                thisParamType(),
                name = ctx.makeName("this"),
                binder = Binder(Identifier(expression.location, ctx.makeName("this")))
        )
    }

    private fun lowerPointerCast(expression: Expression.PointerCast): HIRExpression {
        val value = lowerExpression(expression.arg)
        require(value.type is Type.Ptr)
        val type = lowerTypeAnnotation(expression.toType)
        return HIRExpression.PointerCast(
            expression.location,
            toPointerOfType = type,
            value = value
        )
    }

    private fun lowerDerefExpression(expression: Expression.Deref): HIRExpression {
        return when (val pointerType = ctx.analyzer.typeOfExpression(expression.expression)) {
            is Type.Ptr -> {
                HIRExpression.Load(
                    expression.location,
                    pointerType.to,
                    lowerExpression(expression.expression))
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerAddressOfMut(expression: Expression.AddressOfMut): HIRExpression {
        return when (val expr = expression.expression) {
            is Expression.Var -> {
                val binding = ctx.resolver.resolve(expr.name)
                require(binding is Binding.ValBinding)
                HIRExpression.AddressOf(
                    expression.location,
                    expression.type as Type.Ptr,
                    lowerLocalBinder(binding.statement.binder)
                )
            }
            is Expression.Property -> addressOfStructField(expr)
            else -> requireUnreachable()
        }
    }
    private fun addressOfStructField(expr: Expression.Property): HIRExpression {
        return when (val propertyBinding = ctx.analyzer.resolvePropertyBinding(expr)) {
            is PropertyBinding.StructFieldPointer -> {
                val structPtr = lowerExpression(expr.lhs)
                HIRExpression.GetStructFieldPointer(
                    expr.location,
                    Type.Ptr(expr.type, isMutable = true),
                    structPtr,
                    propertyBinding.member.binder.name,
                    propertyBinding.memberIndex
                )
            }
            is PropertyBinding.StructField -> {
                require(expr.lhs is Expression.Var)
                val lhsBinding = ctx.resolver.resolve(expr.lhs.name)
                require(lhsBinding is Binding.ValBinding)
                val structPtr = HIRExpression.AddressOf(
                    expr.location,
                    Type.Ptr(expr.lhs.type, isMutable = true),
                    lowerLocalBinder(lhsBinding.statement.binder)
                )
                HIRExpression.GetStructFieldPointer(
                    expr.location,
                    Type.Ptr(expr.type, isMutable = true),
                    structPtr,
                    propertyBinding.member.binder.name,
                    propertyBinding.memberIndex
                )
            }
            else -> requireUnreachable()
        }
    }

    private fun lowerAddressOfExpression(expression: Expression.AddressOf): HIRExpression {
        return when (val expr = expression.expression) {
            is Expression.Var -> {
                val binding = ctx.resolver.resolve(expr.name)
                require(binding is Binding.ValBinding)
                HIRExpression.AddressOf(
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

    private fun lowerSizeOfExpression(expression: Expression.SizeOf): HIRExpression {
        return HIRExpression.SizeOf(
                expression.location,
                typeOfExpression(expression),
                lowerTypeAnnotation(expression.type))
    }

    override fun declareVariable(namePrefix: String, type: Type, location: SourceLocation): HIRExpression.ValRef {
        val name = ctx.makeUniqueName(namePrefix)
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
    private fun lowerIfExpression(expression: Expression.If): HIRExpression {
        val result = declareVariable("if_result", typeOfExpression(expression))
        val name = result.name
        val trueBlock = buildBlock(expression.trueBranch.location) {
            emit(HIRStatement.Assignment(
                expression.location,
                name,
                lowerExpression(expression.trueBranch)
            ))
        }
        val falseBlock = buildBlock(expression.falseBranch.location) {
            emit(HIRStatement.Assignment(
                expression.location,
                name,
                lowerExpression(expression.falseBranch)
            ))
        }
        emit(ifStatement(
                expression.location,
                lowerExpression(expression.condition),
                trueBlock,
                falseBlock
        ))
        return result
    }

    private fun lowerNullPtr(expression: Expression.NullPtr): HIRExpression {
        return HIRExpression.NullPtr(expression.location, typeOfExpression(expression) as Type.Ptr)
    }

    private fun lowerBinaryExpression(expression: Expression.BinaryOperation): HIRExpression {
        return HIRExpression.BinOp(
                expression.location,
                typeOfExpression(expression),
                lowerExpression(expression.lhs),
                expression.operator,
                lowerExpression(expression.rhs)
        )
    }

    private fun lowerNotExpression(expression: Expression.Not): HIRExpression {
        return HIRExpression.Not(lowerExpression(expression.expression))

    }

    private fun lowerIntLiteral(expression: Expression.IntLiteral): HIRExpression {
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

    private fun lowerBoolLiteral(expression: Expression.BoolLiteral): HIRExpression {
        return HIRConstant.BoolValue(
            expression.location,
            typeOfExpression(expression),
            expression.value
        )
    }

    private fun lowerPropertyExpression(expression: Expression.Property): HIRExpression = when(val binding = ctx.analyzer.resolvePropertyBinding(expression)) {
        null -> requireUnreachable()
        is PropertyBinding.Global -> exprGen.lowerBinding(expression, binding.binding)
        is PropertyBinding.StructField -> lowerStructFieldBinding(expression, binding)
        is PropertyBinding.StructFieldPointer -> lowerStructFieldPointer(expression, binding)
        is PropertyBinding.ExtensionDef -> lowerExtensionPropertyBinding(expression, binding)
        is PropertyBinding.WhereParamRef -> TODO()
        is PropertyBinding.EnumTypeCaseConstructor -> lowerEnumCaseConstructor(expression, binding)
        is PropertyBinding.WhenCaseFieldRef -> lowerWhenCaseFieldRef(expression, binding)
        is PropertyBinding.TraitFunctionRef -> lowerTraitFunctionRef(expression, binding)
    }

    private fun lowerTraitFunctionRef(expression: Expression.Property, binding: PropertyBinding.TraitFunctionRef): HIRExpression {
        return HIRExpression.TraitMethodRef(
            expression.location,
            expression.type,
            traitName = binding.traitName,
            traitArgs = binding.args,
            methodName = binding.methodName,
        )
    }

    private fun lowerExtensionPropertyBinding(expression: Expression.Property, binding: PropertyBinding.ExtensionDef): HIRExpression {
        return HIRExpression.GlobalRef(
            expression.location,
            binding.type,
            extensionMethodName(binding.extensionDef, binding.functionDef)
        )
    }


    private fun lowerWhenCaseFieldRef(expression: Expression.Property, binding: PropertyBinding.WhenCaseFieldRef): HIRExpression {
        return HIRExpression.GetStructField(
            expression.location,
            typeOfExpression(expression),
            HIRExpression.ValRef(
                expression.lhs.location,
                caseType(binding),
                binding.name.name,
            ),
            binding.propertyName.name,
            binding.propertyIndex + 1, // 0th field is tag
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

    private fun lowerEnumCaseConstructor(expression: Expression.Property, binding: PropertyBinding.EnumTypeCaseConstructor): HIRExpression {
        require(expression.lhs is Expression.Var)
        val name = ctx.resolver.qualifiedName(binding.declaration.name).append(binding.case.name.identifier.name).append(ctx.makeName("constructor"))

        return HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            name
        )
    }

    private fun lowerStructFieldPointer(expression: Expression.Property, binding: PropertyBinding.StructFieldPointer): HIRExpression {
        val expressionType = expression.type
        val structPtrType = expression.lhs.type
        require(structPtrType is Type.Ptr)
        val fieldPtr = HIRExpression.GetStructFieldPointer(
            expression.location,
            structPtrType,
            lowerExpression(expression.lhs),
            memberName = expression.property.name,
            memberIndex = binding.memberIndex
        )
        return HIRExpression.Load(
            expression.location,
            expressionType,
            fieldPtr
        )
    }

    private fun lowerStructFieldBinding(
            expression: Expression.Property,
            binding: PropertyBinding.StructField
    ): HIRExpression {
        return HIRExpression.GetStructField(
            expression.location,
            typeOfExpression(expression),
            lowerExpression(expression.lhs),
            name = expression.property.name,
            index = binding.memberIndex
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

    private fun lowerByteString(expression: Expression.ByteString): HIRExpression {
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