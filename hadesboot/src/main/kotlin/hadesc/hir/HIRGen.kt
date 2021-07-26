package hadesc.hir

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.context.HasContext
import hadesc.diagnostics.Diagnostic
import hadesc.frontend.PropertyBinding
import hadesc.hir.transformers.ParamToLocal
import hadesc.ir.passes.TypeTransformer
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type
import libhades.collections.Stack
import llvm.makeList

@Suppress("SimplifiableCallChain")
class HIRGen(
        override val ctx: Context
): HasContext {
    private val paramToLocal = ParamToLocal(ctx)
    fun lowerSourceFiles(sourceFiles: Collection<SourceFile>): HIRModule {
        val declarations = mutableListOf<HIRDefinition>()
        for (sourceFile in sourceFiles) {
            for (it in sourceFile.declarations) {
                declarations.addAll(lowerDeclaration(it))
            }
        }
        val result = HIRModule(declarations)
        logger().debug("HIRGen output")
        logger().debug(result.prettyPrint())
        return result
    }

    private fun lowerDeclaration(declaration: Declaration): List<HIRDefinition> = when (declaration) {
        is Declaration.Error -> requireUnreachable()
        is Declaration.ImportAs -> emptyList()
        is Declaration.FunctionDef -> listOf(lowerFunctionDef(declaration))
        is Declaration.ConstDefinition -> lowerConstDef(declaration)
        is Declaration.ExternFunctionDef -> lowerExternFunctionDef(declaration)
        is Declaration.Struct -> lowerStructDef(declaration)
        is Declaration.TypeAlias -> emptyList()
        is Declaration.ExtensionDef -> lowerExtensionDef(declaration)
        is Declaration.TraitDef -> emptyList()
        is Declaration.ImplementationDef -> lowerImplementationDef(declaration)
        is Declaration.ImportMembers -> emptyList()
        is Declaration.SealedType -> lowerSealedType(declaration)
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
                    ctx.resolver.qualifiedName(declaration.name).append(functionDef.name.identifier.name)))
        }
        currentExtensionDef = null
        return list
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

    private fun lowerSealedType(declaration: Declaration.SealedType): List<HIRDefinition> {
        val sealedTypeName = lowerGlobalName(declaration.name)
        val caseStructs = declaration.cases.flatMapIndexed { index, case -> listOf(
            HIRDefinition.Struct(
                case.name.location,
                sealedTypeName.append(case.name.identifier.name),
                typeParams = declaration.typeParams?.map { HIRTypeParam(it.location, it.binder.identifier.name) },
                fields = listOf(ctx.makeName("\$tag") to ctx.sealedTypeDiscriminantType()) + (case.params?.map {
                    it.binder.identifier.name to ctx.analyzer.annotationToType(requireNotNull(it.annotation))
                } ?: emptyList())
            ),
            HIRDefinition.Const(
                case.name.location,
                sealedTypeName.append(case.name.identifier.name).append(ctx.makeName("tag")),
                HIRExpression.Constant(
                    HIRConstant.IntValue(
                        case.name.location,
                        ctx.sealedTypeDiscriminantType(),
                        index
                    )
                )
            ),
            sealedCaseConstructorOrConst(declaration, sealedTypeName, case, index)
        )}.toTypedArray()
        return listOf(
            *caseStructs,
            HIRDefinition.Struct(
                declaration.location,
                lowerGlobalName(declaration.name),
                typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                fields = listOf(
                    ctx.makeName("\$tag") to ctx.sealedTypeDiscriminantType(),
                    ctx.makeName("payload") to ctx.analyzer.getSealedTypePayloadType(declaration)
                )
            )
        )
    }

    private fun sealedCaseConstructorOrConst(
        declaration: Declaration.SealedType,
        sealedTypeName: QualifiedName,
        case: Declaration.SealedType.Case,
        index: Int
    ): HIRDefinition {
        val caseConstructorName = sealedTypeName.append(case.name.name).append(ctx.makeName("constructor"))
        val typeConstructor = Type.Constructor(binder = null, name = ctx.resolver.qualifiedName(declaration.name))
        val instanceType = if (declaration.typeParams != null) {
            Type.Application(
                typeConstructor,
                declaration.typeParams.map { Type.ParamRef(it.binder) }
            )
        } else {
            typeConstructor
        }
        val caseStructName = ctx.resolver.qualifiedName(declaration.name).append(case.name.name)
        val caseTypeConstructor = Type.Constructor(binder = null, name = caseStructName)
        val caseInstanceType = if (declaration.typeParams != null) {
            Type.Application(
                caseTypeConstructor,
                declaration.typeParams.map { Type.ParamRef(it.binder) }
            )
        } else {
            caseTypeConstructor
        }
        val loc = case.name.location
        val tag = HIRExpression.Constant(
            HIRConstant.IntValue(loc, ctx.sealedTypeDiscriminantType(), index)
        )
        val caseValName = ctx.makeUniqueName()
        val resultName = ctx.makeUniqueName()
        val caseConstructorRef = HIRExpression.GlobalRef(
            loc,
            type = Type.Bool, // NOCOMMIT
            name = ctx.resolver.qualifiedName(declaration.name).append(case.name.name)
        )
        val caseConstructorTypeApplication = if (declaration.typeParams != null) {
            HIRExpression.TypeApplication(
                loc,
                type = Type.Bool, // NOCOMMIT
                expression = caseConstructorRef,
                args = declaration.typeParams.map { Type.ParamRef(it.binder) }
            )
        } else {
            caseConstructorRef
        }
        val caseConstructorCall = HIRExpression.Call(
            loc,
            caseInstanceType,
            caseConstructorTypeApplication,
            listOf(tag) + (case.params?.map {
                HIRExpression.ParamRef(
                    it.location,
                    lowerTypeAnnotation(requireNotNull(it.annotation)),
                    it.binder.name,
                    it.binder
                )
            } ?: emptyList())
        )
        val addressOfCaseInstance = HIRExpression.AddressOf(
            loc,
            Type.Ptr(caseInstanceType, isMutable = false),
            caseValName
        )
        val castedValue = HIRExpression.Load(
            loc,
            instanceType,
            HIRExpression.PointerCast(
                loc,
                instanceType,
                addressOfCaseInstance
            )
        )
        val statements = listOf(
            HIRStatement.ValDeclaration(
                loc,
                caseValName,
                isMutable = false,
                caseInstanceType
            ),
            HIRStatement.ValDeclaration(
                loc,
                resultName,
                isMutable = false,
                instanceType
            ),
            HIRStatement.Assignment(
                loc,
                caseValName,
                caseConstructorCall,
            ),
            HIRStatement.Assignment(
                loc,
                resultName,
                castedValue
            ),
            HIRStatement.Return(
                loc,
                HIRExpression.ValRef(loc, instanceType, resultName)
            )
        )
        return HIRDefinition.Function(
            case.name.location,
            signature = HIRFunctionSignature(
                case.name.location,
                caseConstructorName,
                typeParams = declaration.typeParams?.map { HIRTypeParam(it.location, it.binder.name) },
                params = case.params?.map {
                    HIRParam(
                        it.location,
                        it.binder,
                        lowerTypeAnnotation(requireNotNull(it.annotation)))
                } ?: emptyList(),
                returnType = instanceType
            ),
            HIRBlock(case.name.location, statements)
        )
    }


    private fun lowerExternFunctionDef(declaration: Declaration.ExternFunctionDef): List<HIRDefinition> {
        return listOf(
                HIRDefinition.ExternFunction(
                        declaration.location,
                        lowerGlobalName(declaration.binder),
                        params = declaration.paramTypes.map { lowerTypeAnnotation(it) },
                        externName = declaration.externName.name,
                        returnType = lowerTypeAnnotation(declaration.returnType)
                )
        )
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
        )
        currentFunctionDef = null
        return HIRDefinition.Function(
                location = declaration.location,
                signature = signature,
                body = body
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
        header.forEach { addStatement(it) }
        for (member in body.members) {
            lowerBlockMember(member).forEach {
                addStatement(it)
            }
        }
        if (addReturnVoid) {
            terminateScope()
            addStatement(HIRStatement.ReturnVoid(body.location))
        } else {
            for (statement in requireNotNull(deferStack.peek())) {
                addStatement(statement)
            }
        }
        deferStack.pop()
    }

    private fun buildBlock(location: SourceLocation, f: () -> Unit): HIRBlock {
        val oldStatements = currentStatements
        val statements = mutableListOf<HIRStatement>()
        currentStatements = statements
        f()
        currentStatements = oldStatements
        return HIRBlock(location, statements)
    }

    private fun addStatement(statement: HIRStatement) {
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
        return listOf(
                HIRStatement.While(
                        statement.location,
                        lowerExpression(statement.condition),
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

    private fun lowerLocalBinder(binder: Binder): Name {
        // TODO: Handle shadowing
        return binder.identifier.name
    }

    private fun lowerIfStatement(statement: Statement.If): Collection<HIRStatement> {
        return listOf(
                HIRStatement.If(
                        location = statement.location,
                        condition = lowerExpression(statement.condition),
                        trueBranch = lowerBlock(statement.ifTrue),
                        falseBranch = statement.ifFalse?.let { lowerBlock(it) } ?: HIRBlock(
                                location = statement.location,
                                statements = emptyList()
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
                addStatement(statement)
            }
        }
    }

    private fun lowerExpression(expression: Expression): HIRExpression = postLowerExpression(when(expression) {
        is Expression.Error -> requireUnreachable()
        is Expression.Var -> lowerVarExpression(expression)
        is Expression.Call -> lowerCallExpression(expression)
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
        is Expression.When -> lowerWhenExpression(expression)
        is Expression.As -> lowerAsExpression(expression)
    })

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

    private fun lowerWhenExpression(expression: Expression.When): HIRExpression {
        val value = lowerExpression(expression.value)
        val discriminantType = value.type
        val discriminants = ctx.analyzer.getDiscriminants(discriminantType)
        val declaration = ctx.analyzer.getSealedTypeDeclaration(discriminantType)
        val cases = makeList {
            for (discriminant in discriminants) {
                val arm = expression.arms.find {
                    it is Expression.WhenArm.Is && it.caseName.name == discriminant.name
                }
                val casePayloadTypeConstructor =
                    Type.Constructor(null, ctx.resolver.qualifiedName(declaration.name).append(discriminant.name))
                val casePayloadType = if (declaration.typeParams == null) {
                    casePayloadTypeConstructor
                }  else {
                    Type.Application(
                        casePayloadTypeConstructor,
                        discriminantType.typeArgs()
                    )
                }
                if (arm == null) {
                    val elseArm = requireNotNull(expression.arms.find {
                        it is Expression.WhenArm.Else
                    })
                    require(elseArm is Expression.WhenArm.Else)
                    add(HIRExpression.When.Case(
                        casePayloadType,
                        discriminant.name,
                        ctx.makeUniqueName(),
                        lowerExpression(elseArm.value)
                    ))
                } else {
                    require(arm is Expression.WhenArm.Is)
                    add(HIRExpression.When.Case(
                        casePayloadType,
                        discriminant.name,
                        arm.name?.identifier?.name ?: ctx.makeUniqueName(),
                        lowerExpression(arm.value)

                    ))
                }
            }
        }
        return HIRExpression.When(
            expression.location,
            typeOfExpression(expression),
            discriminant = lowerExpression(expression.value),
            cases = cases
        )

    }

    private fun lowerTypeApplication(expression: Expression.TypeApplication): HIRExpression {

        val typeApplication = HIRExpression.TypeApplication(
            expression.location,
            typeOfExpression(expression),
            expression = lowerExpression(expression.lhs),
            args = checkNotNull(ctx.analyzer.getTypeArgs(expression.lhs))
        )
        if (ctx.analyzer.isSealedTypeConstructorCall(expression)) {
            return HIRExpression.Call(
                expression.location,
                typeOfExpression(expression),
                callee = typeApplication,
                args = emptyList()
            )
        }
        return typeApplication
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
        val expr = expression.expression
        return when (expr) {
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
        val expr = expression.expression
        return when (expr) {
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

    private fun lowerIfExpression(expression: Expression.If): HIRExpression {
        val name = ctx.makeUniqueName()
        addStatement(HIRStatement.ValDeclaration(
                expression.location,
                name,
                type = typeOfExpression(expression),
                isMutable = false)
        )
        val trueBlock = buildBlock(expression.trueBranch.location) {
            addStatement(HIRStatement.Assignment(
                expression.location,
                name,
                lowerExpression(expression.trueBranch)
            ))
        }
        val falseBlock = buildBlock(expression.falseBranch.location) {
            addStatement(HIRStatement.Assignment(
                expression.location,
                name,
                lowerExpression(expression.falseBranch)
            ))
        }
        addStatement(HIRStatement.If(
                expression.location,
                lowerExpression(expression.condition),
                trueBlock,
                falseBlock
        ))
        return HIRExpression.ValRef(
            expression.location,
            typeOfExpression(expression),
            name,
        )
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
        return HIRExpression.Constant(
                HIRConstant.IntValue(
                        expression.location,
                        typeOfExpression(expression),
                        expression.value)
        )
    }

    private fun lowerBoolLiteral(expression: Expression.BoolLiteral): HIRExpression {
        return HIRExpression.Constant(HIRConstant.BoolValue(
                expression.location,
                typeOfExpression(expression),
                expression.value
        ))
    }

    private fun lowerPropertyExpression(expression: Expression.Property): HIRExpression = when(val binding = ctx.analyzer.resolvePropertyBinding(expression)) {
        null -> requireUnreachable()
        is PropertyBinding.Global -> lowerBinding(expression, binding.binding)
        is PropertyBinding.StructField -> lowerStructFieldBinding(expression, binding)
        is PropertyBinding.StructFieldPointer -> lowerStructFieldPointer(expression, binding)
        is PropertyBinding.ExtensionDef -> requireUnreachable()
        is PropertyBinding.WhereParamRef -> TODO()
        is PropertyBinding.SealedTypeCaseConstructor -> lowerSealedTypeCaseConstructor(expression, binding)
        is PropertyBinding.WhenCaseFieldRef -> lowerWhenCaseFieldRef(expression, binding)
        is PropertyBinding.TraitFunctionRef -> requireUnreachable()
        else -> TODO()
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
            binder = null,
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

    private fun lowerSealedTypeCaseConstructor(expression: Expression.Property, binding: PropertyBinding.SealedTypeCaseConstructor): HIRExpression {
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

    private fun lowerBinding(
            expression: Expression,
            binding: Binding
    ): HIRExpression = when(binding) {
        is Binding.GlobalFunction -> HIRExpression.GlobalRef(
                expression.location,
                typeOfExpression(expression),
                lowerGlobalName(binding.declaration.name)
        )
        is Binding.ExternFunction -> HIRExpression.GlobalRef(
                expression.location,
                typeOfExpression(expression),
                lowerGlobalName(binding.declaration.binder)
        )
        is Binding.FunctionParam -> HIRExpression.ParamRef(
                expression.location,
                typeOfExpression(expression),
                lowerLocalBinder(binding.param.binder),
                binding.binder
        )
        is Binding.ValBinding -> HIRExpression.ValRef(
                expression.location,
                typeOfExpression(expression),
                lowerLocalBinder(binding.statement.binder)
        )
        is Binding.Struct -> HIRExpression.GlobalRef(
                expression.location,
                typeOfExpression(expression),
                lowerGlobalName(binding.declaration.binder)
        )
        is Binding.GlobalConst -> HIRExpression.GlobalRef(
                expression.location,
                typeOfExpression(expression),
                lowerGlobalName(binding.declaration.name)
        )
        is Binding.ClosureParam -> HIRExpression.ParamRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.param.binder),
            binding.binder,
        )
        is Binding.SealedType -> TODO()
        is Binding.WhenArm -> requireUnreachable()
        is Binding.ExternConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
    }

    private fun lowerByteString(expression: Expression.ByteString): HIRExpression {
        return HIRExpression.Constant(
                HIRConstant.ByteString(
                        expression.location,
                        typeOfExpression(expression),
                        expression.bytes))
    }

    private val typeOfExpressionCache = mutableMapOf<SourceLocation, Type>()
    private fun typeOfExpression(expression: Expression): Type = typeOfExpressionCache.getOrPut(expression.location) {
        val type = ctx.analyzer.reduceGenericInstances(ctx.analyzer.typeOfExpression(expression))
        checkUninferredGenerics(expression, type)
        return type
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
            else -> ctx.diagnosticReporter.report(node.location, Diagnostic.Kind.UninferrableTypeParam(instance.name))
        }
    }

    private fun lowerVarExpression(expression: Expression.Var): HIRExpression {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> requireUnreachable {
                "Found unresolved variable: ${expression.name} at ${expression.location}"
            }
            else -> lowerBinding(expression, binding)
        }
    }

    private fun lowerCallExpression(expression: Expression.Call): HIRExpression {
        if (expression.callee is Expression.Property) {
            when(val binding = ctx.analyzer.resolvePropertyBinding(expression.callee)) {
                is PropertyBinding.ExtensionDef -> {
                    val extensionDefName = ctx.resolver.qualifiedName(binding.extensionDef.name)
                    val extensionMethod = binding.extensionDef.functionDefs[binding.functionIndex]
                    val fullName = extensionDefName.append(extensionMethod.name.identifier.name)
                    return buildCall(
                        expression,
                        callee = HIRExpression.GlobalRef(
                            expression.location,
                            typeOfExpression(expression),
                            fullName
                        ),
                        args = listOf(lowerExpression(expression.callee.lhs))
                                + expression.args.map { lowerExpression(it.expression) }
                    )
                }
                is PropertyBinding.TraitFunctionRef -> {
                    return HIRExpression.TraitMethodCall(
                        expression.location,
                        typeOfExpression(expression),
                        methodName = expression.callee.property.name,
                        traitName = binding.traitName,
                        traitArgs = binding.args,
                        args = expression.args.map { lowerExpression(it.expression) },
                    )
                }
            }
        }
        if (expression.callee.type is Type.Function) {
            require(expression.typeArgs == null) {
                TODO("Closures with type arguments not implemented")
            }
            return HIRExpression.InvokeClosure(
                location = expression.location,
                type = expression.type,
                closure = lowerExpression(expression.callee),
                args = expression.args.map { lowerExpression(it.expression)}
            )
        }
        return buildCall(
                expression,
                callee = lowerExpression(expression.callee),
                args = expression.args.map { lowerExpression(it.expression) }
        )
    }

    private fun buildCall(
        call: Expression,
        callee: HIRExpression,
        args: List<HIRExpression>
    ): HIRExpression {
        val type = typeOfExpression(call)
        val typeArgs = ctx.analyzer.getTypeArgs(call)
        typeArgs?.forEach {
            checkUninferredGenerics(call, it)
        }
        return if (typeArgs != null) {
            val reducedArgs = typeArgs.map { ctx.analyzer.reduceGenericInstances(it) }
            val appliedType = applyType(ctx.analyzer.reduceGenericInstances(callee.type), reducedArgs)
            HIRExpression.Call(
                call.location,
                type,
                HIRExpression.TypeApplication(
                    location = callee.location,
                    type = appliedType,
                    args = reducedArgs,
                    expression = callee
                ),
                args
            )
        } else {
            HIRExpression.Call(
                call.location,
                type,
                callee,
                args
            )
        }
    }

    private fun applyType(type: Type, args: List<Type>): Type {
        if (type !is Type.TypeFunction) return type
        require(type.params.size == args.size)
        return type.body.applySubstitution(
            type.params.zip(args).map {
                it.first.binder.location to ctx.analyzer.reduceGenericInstances(it.second)
            }.toMap()
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

    private fun lowerGlobalName(binder: Binder): QualifiedName {
        val binding = requireNotNull(ctx.resolver.resolve(binder.identifier))
        if (binding is Binding.GlobalFunction && binding.declaration.externName != null) {
            return QualifiedName(listOf(binding.declaration.externName.name))
        }
        return ctx.resolver.resolveGlobalName(binder)
    }
}