package hadesc.hir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.checker.ImplementationBinding
import hadesc.checker.PropertyBinding
import hadesc.context.Context
import hadesc.location.SourceLocation
import hadesc.logging.logger
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type

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
        logger().debug(result.prettyPrint())
        return result
    }

    private fun lowerDeclaration(declaration: Declaration): List<HIRDefinition> = when (declaration) {
        is Declaration.Error -> requireUnreachable()
        is Declaration.ImportAs -> emptyList()
        is Declaration.FunctionDef -> lowerFunctionDef(declaration)
        is Declaration.ConstDefinition -> TODO()
        is Declaration.ExternFunctionDef -> lowerExternFunctionDef(declaration)
        is Declaration.Struct -> lowerStructDef(declaration)
        is Declaration.Interface -> TODO()
        is Declaration.Implementation -> TODO()
        is Declaration.Enum -> TODO()
        is Declaration.TypeAlias -> emptyList()
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

    private fun lowerFunctionDef(declaration: Declaration.FunctionDef): List<HIRDefinition> {
        val returnType = lowerTypeAnnotation(declaration.signature.returnType)
        val addReturnVoid = returnType is Type.Void && !hasTerminator(declaration.body)
        val loweredDef = HIRDefinition.Function(
                location = declaration.location,
                receiverType = declaration.signature.thisParam?.annotation?.let { lowerTypeAnnotation(it) },
                name = lowerGlobalName(declaration.name),
                typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
                params = declaration.params.map { lowerParam(it) },
                returnType = returnType,
                body = lowerBlock(declaration.body, addReturnVoid)
        )

        return listOf(loweredDef)
    }

    private fun hasTerminator(body: Block): Boolean {
        if (body.members.isEmpty()) {
            return false
        }
        val last = body.members.last()

        return last is Block.Member.Statement && last.statement is Statement.Return
    }

    private var currentStatements: MutableList<HIRStatement>? = null
    private fun lowerBlock(body: Block, addReturnVoid: Boolean = false): HIRBlock = buildBlock(body.location) {
        for (member in body.members) {
            lowerBlockMember(member).forEach {
                addStatement(it)
            }
        }
        if (addReturnVoid) {
            addStatement(HIRStatement.ReturnVoid(body.location))
        }
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
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Defer -> TODO()
        is Statement.Error -> requireUnreachable()
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

    private fun lowerExpression(expression: Expression): HIRExpression = when(expression) {
        is Expression.Error -> requireUnreachable()
        is Expression.Var -> lowerVarExpression(expression)
        is Expression.Call -> lowerCallExpression(expression)
        is Expression.Property -> lowerPropertyExpression(expression)
        is Expression.ByteString -> lowerByteString(expression)
        is Expression.BoolLiteral -> lowerBoolLiteral(expression)
        is Expression.This -> lowerThisExpression(expression)
        is Expression.NullPtr -> lowerNullPtr(expression)
        is Expression.IntLiteral -> lowerIntLiteral(expression)
        is Expression.Not -> lowerNotExpression(expression)
        is Expression.BinaryOperation -> lowerBinaryExpression(expression)
        is Expression.SizeOf -> lowerSizeOfExpression(expression)
        is Expression.AddressOf -> lowerAddressOfExpression(expression)
        is Expression.AddressOfMut -> lowerAddressOfMut(expression)
        is Expression.Deref -> TODO()
        is Expression.PointerCast -> TODO()
        is Expression.If -> lowerIfExpression(expression)
        is Expression.TypeApplication -> TODO()
        is Expression.Match -> TODO()
        is Expression.New -> TODO()
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

    private fun lowerThisExpression(expression: Expression.This): HIRExpression {
        return HIRExpression.ThisRef(
                expression.location,
                typeOfExpression(expression)
        )
    }

    private fun lowerBoolLiteral(expression: Expression.BoolLiteral): HIRExpression {
        return HIRExpression.Constant(HIRConstant.BoolValue(
                expression.location,
                typeOfExpression(expression),
                expression.value
        ))
    }

    private fun lowerPropertyExpression(expression: Expression.Property): HIRExpression = when(val binding = ctx.checker.getPropertyBinding(expression)) {
        is PropertyBinding.Global -> lowerBinding(expression, binding.binding)
        is PropertyBinding.StructField -> lowerStructFieldBinding(expression, binding)
        is PropertyBinding.StructFieldPointer -> TODO()
        is PropertyBinding.GlobalExtensionFunction -> lowerGlobalExtensionRef(expression, binding)
        is PropertyBinding.InterfaceExtensionFunction -> lowerInterfaceMethodRef(expression, binding)
        null -> requireUnreachable()
    }

    private fun lowerInterfaceMethodRef(
            expression: Expression.Property,
            binding: PropertyBinding.InterfaceExtensionFunction
    ): HIRExpression = when (val interfaceBinding = binding.implementationBinding) {
        is ImplementationBinding.GlobalImpl -> lowerGlobalImplRef(expression, binding, interfaceBinding)
        is ImplementationBinding.TypeBound -> TODO()
        is ImplementationBinding.ImplParamTypeBound -> TODO()
    }

    private fun lowerGlobalImplRef(
            expression: Expression.Property,
            propertyBinding: PropertyBinding.InterfaceExtensionFunction,
            interfaceBinding: ImplementationBinding.GlobalImpl
    ): HIRExpression {
        return HIRExpression.MethodRef(
                location = expression.location,
                type = ctx.checker.typeOfExpression(expression),
                thisValue = lowerExpression(expression.lhs),
                propertyBinding = HIRPropertyBinding.ImplementationMethodRef(
                        expression.location,
                        implName = implName(interfaceBinding.implDef),
                        interfaceMemberIndex = propertyBinding.memberIndex
                )
        )
    }

    private fun implName(implDef: Declaration.Implementation): QualifiedName {
        val interfaceDef = ctx.resolver.resolveDeclaration(implDef.interfaceRef.path)
        require(interfaceDef is Declaration.Interface)
        val qualifiedInterfaceName = ctx.resolver.qualifiedInterfaceName(interfaceDef)
        require(implDef.typeParams == null)
        return QualifiedName(listOf(
                ctx.makeName("impl"),
                *qualifiedInterfaceName.names.toTypedArray(),
                ctx.makeName("for"),
                ctx.makeName(lowerTypeAnnotation(implDef.forType).prettyPrint())
        ))
    }

    private fun lowerGlobalExtensionRef(
            expression: Expression.Property,
            binding: PropertyBinding.GlobalExtensionFunction
    ): HIRExpression {
        return HIRExpression.MethodRef(
                location = expression.location,
                type = typeOfExpression(expression),
                thisValue = lowerExpression(expression.lhs),
                propertyBinding = HIRPropertyBinding.GlobalExtensionRef(
                        location = expression.lhs.location,
                        functionName = lowerGlobalName(binding.def.name)
                )
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
        is Binding.GlobalConst -> TODO()
        is Binding.EnumCaseConstructor -> TODO()
        is Binding.Pattern -> TODO()
    }

    private fun lowerByteString(expression: Expression.ByteString): HIRExpression {
        return HIRExpression.Constant(
                HIRConstant.ByteString(
                        expression.location,
                        typeOfExpression(expression),
                        expression.bytes))
    }

    private fun typeOfExpression(expression: Expression): Type {
        return ctx.checker.typeOfExpression(expression)
    }

    private fun lowerVarExpression(expression: Expression.Var): HIRExpression {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> requireUnreachable()
            else -> lowerBinding(expression, binding)
        }
    }

    private fun lowerCallExpression(expression: Expression.Call): HIRExpression {
        return HIRExpression.Call(
                expression.location,
                typeOfExpression(expression),
                lowerExpression(expression.callee),
                ctx.checker.getTypeArgs(expression),
                expression.args.map { lowerExpression(it.expression) }
        )
    }

    private fun lowerTypeAnnotation(annotation: TypeAnnotation): Type {
        return ctx.checker.annotationToType(annotation)
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
        return ctx.resolver.resolveGlobalName(binder)
    }
}