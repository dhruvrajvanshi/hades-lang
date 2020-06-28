package hadesc.hir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.checker.PropertyBinding
import hadesc.context.Context
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
        return HIRModule(declarations)
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

    private fun lowerBlock(body: Block, addReturnVoid: Boolean = false): HIRBlock {
        val statements = mutableListOf<HIRStatement>()
        for (member in body.members) {
            statements.addAll(lowerBlockMember(member))
        }
        if (addReturnVoid) {
            statements.add(HIRStatement.ReturnVoid(body.location))
        }
        return HIRBlock(body.location, statements)
    }

    private fun lowerBlockMember(member: Block.Member): Collection<HIRStatement> = when(member) {
        is Block.Member.Expression -> listOf(HIRStatement.Expression(lowerExpression(member.expression)))
        is Block.Member.Statement -> lowerStatement(member.statement)
    }

    private fun lowerStatement(statement: Statement): Collection<HIRStatement> = when(statement) {
        is Statement.Return -> lowerReturnStatement(statement)
        is Statement.Val -> lowerValStatement(statement)
        is Statement.While -> TODO()
        is Statement.If -> lowerIfStatement(statement)
        is Statement.LocalAssignment -> TODO()
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Defer -> TODO()
        is Statement.Error -> requireUnreachable()
    }

    private fun lowerValStatement(statement: Statement.Val): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Val(
                        statement.location,
                        lowerLocalBinder(statement.binder),
                        statement.isMutable,
                        lowerExpression(statement.rhs)
                )
        )
    }

    private fun lowerLocalBinder(binder: Binder): Name {
        // TODO: Handle shadowing
        return binder.identifier.name
    }

    private fun lowerIfStatement(statement: Statement.If): Collection<HIRStatement> {
        TODO()
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
        is Expression.NullPtr -> TODO()
        is Expression.IntLiteral -> TODO()
        is Expression.Not -> TODO()
        is Expression.BinaryOperation -> TODO()
        is Expression.SizeOf -> TODO()
        is Expression.AddressOf -> TODO()
        is Expression.AddressOfMut -> TODO()
        is Expression.Load -> TODO()
        is Expression.PointerCast -> TODO()
        is Expression.If -> TODO()
        is Expression.TypeApplication -> TODO()
        is Expression.Match -> TODO()
        is Expression.New -> TODO()
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
        is PropertyBinding.GlobalExtensionFunction -> TODO()
        is PropertyBinding.InterfaceExtensionFunction -> TODO()
        null -> requireUnreachable()
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
        TODO()
    }

    private fun lowerGlobalName(binder: Binder): QualifiedName {
        return ctx.resolver.resolveGlobalName(binder)
    }
}