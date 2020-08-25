package hadesc.hir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.frontend.PropertyBinding
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.ir.IRVariable
import hadesc.ir.passes.TypeTransformer
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type
import libhades.collections.Stack

class HIRGen(
        private val ctx: Context
) {
    fun lowerSourceFiles(sourceFiles: Collection<SourceFile>): HIRModule {
        val declarations = mutableListOf<HIRDefinition>()
        for (sourceFile in sourceFiles) {
            for (it in sourceFile.declarations) {
                declarations.addAll(lowerDeclaration(it))
            }
        }
        val result = HIRModule(declarations)
        return result
    }

    private fun lowerDeclaration(declaration: Declaration): List<HIRDefinition> = when (declaration) {
        is Declaration.Error -> requireUnreachable()
        is Declaration.ImportAs -> emptyList()
        is Declaration.FunctionDef -> listOf(lowerFunctionDef(declaration))
        is Declaration.ConstDefinition -> lowerConstDef(declaration)
        is Declaration.ExternFunctionDef -> lowerExternFunctionDef(declaration)
        is Declaration.Struct -> lowerStructDef(declaration)
        is Declaration.Enum -> TODO()
        is Declaration.TypeAlias -> emptyList()
        is Declaration.ExtensionDef -> lowerExtensionDef(declaration)
        is Declaration.InterfaceDef -> TODO()
        is Declaration.ImplementationDef -> TODO()
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
        val body = lowerBlock(declaration.body, addReturnVoid)
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
                    name = ctx.makeName("this"),
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
                constraintParams = null,
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
    private fun lowerBlock(body: Block, addReturnVoid: Boolean = false): HIRBlock = buildBlock(body.location) {
        deferStack.push(mutableListOf())
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
        val name = binding.statement.binder.identifier.name
        val structAddr = HIRExpression.AddressOf(
                statement.lhs.lhs.location,
                type = Type.Ptr(typeOfExpression(statement.lhs.lhs), isMutable = true),
                name = name
        )
        val fieldBinding = ctx.checker.resolveStructFieldBinding(typeOfExpression(statement.lhs.lhs), statement.lhs.property)
        requireNotNull(fieldBinding)
        val fieldType = fieldBinding.type
        val fieldAddr = HIRExpression.GetStructFieldPointer(
                location = statement.lhs.location,
                type = Type.Ptr(fieldType, isMutable = true),
                lhs = structAddr,
                memberName = statement.lhs.property.name,
                memberIndex = fieldBinding.memberIndex
        )
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
                        typeOfExpression(statement.rhs)
                ),
                HIRStatement.Assignment(
                        statement.location,
                        name,
                        lowerExpression(statement.rhs)
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

    private fun lowerExpression(expression: Expression): HIRExpression = when(expression) {
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
        is Expression.TypeApplication -> TODO()
        is Expression.Match -> TODO()
        is Expression.New -> TODO()
        is Expression.PipelineOperator -> lowerPipelineOperator(expression)
        is Expression.This -> lowerThisExpression(expression)
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
                name = ctx.makeName("this")
        )
    }

    private fun lowerPipelineOperator(expression: Expression.PipelineOperator): HIRExpression {
        return buildCall(
            call = expression,
            args = listOf(lowerExpression(expression.lhs)),
            callee = lowerExpression(expression.rhs)
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
        val pointerType = ctx.checker.typeOfExpression(expression.expression)
        require(pointerType is Type.Ptr)
        return HIRExpression.Load(
            expression.location,
            pointerType.to,
            lowerExpression(expression.expression))
    }

    private fun lowerAddressOfMut(expression: Expression.AddressOfMut): HIRExpression {
        require(expression.expression is Expression.Var)
        val binding = ctx.resolver.resolve(expression.expression.name)
        require(binding is Binding.ValBinding)
        return HIRExpression.AddressOf(
                expression.location,
                typeOfExpression(expression) as Type.Ptr,
                lowerLocalBinder(binding.statement.binder)
        )
    }

    private fun lowerAddressOfExpression(expression: Expression.AddressOf): HIRExpression {
        require(expression.expression is Expression.Var)
        val binding = ctx.resolver.resolve(expression.expression.name)
        require(binding is Binding.ValBinding)
        return HIRExpression.AddressOf(
                expression.location,
                typeOfExpression(expression) as Type.Ptr,
                lowerLocalBinder(binding.statement.binder)
        )
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
        return HIRExpression.ValRef(expression.location, typeOfExpression(expression), name)
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

    private fun lowerPropertyExpression(expression: Expression.Property): HIRExpression = when(val binding = ctx.checker.resolvePropertyBinding(expression)) {
        null -> requireUnreachable()
        is PropertyBinding.Global -> lowerBinding(expression, binding.binding)
        is PropertyBinding.StructField -> lowerStructFieldBinding(expression, binding)
        is PropertyBinding.StructFieldPointer -> lowerStructFieldPointer(expression, binding)
        is PropertyBinding.ExtensionDef -> requireUnreachable()
        is PropertyBinding.WhereParamRef -> TODO()
    }

    private fun lowerStructFieldPointer(expression: Expression.Property, binding: PropertyBinding.StructFieldPointer): HIRExpression {
        return HIRExpression.GetStructFieldPointer(
                expression.location,
                typeOfExpression(expression),
                lowerExpression(expression.lhs),
                memberName = expression.property.name,
                memberIndex = binding.memberIndex
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
                lowerLocalBinder(binding.param.binder)
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
        is Binding.EnumCaseConstructor -> TODO()
        is Binding.Pattern -> TODO()
        is Binding.WhereParam -> TODO()
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
        val type = ctx.checker.reduceGenericInstances(ctx.checker.typeOfExpression(expression))
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
            null -> requireUnreachable()
            else -> lowerBinding(expression, binding)
        }
    }

    private fun lowerCallExpression(expression: Expression.Call): HIRExpression {
        if (expression.callee is Expression.Property) {
            val binding = ctx.checker.resolvePropertyBinding(expression.callee)
            if (binding is PropertyBinding.ExtensionDef) {
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
        val typeArgs = ctx.checker.getTypeArgs(call)
        typeArgs?.forEach {
            checkUninferredGenerics(call, it)
        }
        return if (typeArgs != null) {
            val reducedArgs = typeArgs.map { ctx.checker.reduceGenericInstances(it) }
            val appliedType = applyType(ctx.checker.reduceGenericInstances(callee.type), reducedArgs)
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
                it.first.binder.location to ctx.checker.reduceGenericInstances(it.second)
            }.toMap()
        )
    }

    private fun lowerTypeAnnotation(annotation: TypeAnnotation): Type {
        return ctx.checker.reduceGenericInstances(ctx.checker.annotationToType(annotation))
    }

    private fun lowerParam(param: Param): HIRParam {
        return HIRParam(
                param.location,
                name = param.binder.identifier.name,
                type = ctx.checker.typeOfBinder(param.binder)
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