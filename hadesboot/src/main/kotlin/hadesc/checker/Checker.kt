package hadesc.checker

import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.typer.Variance
import hadesc.types.Type
import java.util.*

class Checker(private val ctx: Context) {
    private val returnTypeStack = Stack<Type>()
    fun checkDeclaration(declaration: Declaration) = when (declaration) {
        is Declaration.Error -> {}
        is Declaration.ImportAs -> checkImportAsDeclaration(declaration)
        is Declaration.FunctionDef -> checkFunctionDef(declaration)
        is Declaration.ConstDefinition -> TODO()
        is Declaration.ExternFunctionDef -> checkExternFunctionDef(declaration)
        is Declaration.Struct -> checkStructDecl(declaration)
        is Declaration.Interface -> TODO()
        is Declaration.Implementation -> TODO()
        is Declaration.Enum -> TODO()
        is Declaration.TypeAlias -> checkTypeAliasDecl(declaration)
    }

    private fun checkTypeAliasDecl(declaration: Declaration.TypeAlias) {
        declaration.typeParams?.forEach { checkTypeParam(it) }
        checkAnnotation(declaration.rhs)
    }

    private fun checkStructDecl(declaration: Declaration.Struct) {
        checkNameBinding(declaration.binder)
        declaration.typeParams?.forEach {
            checkTypeParam(it)
        }
        for (member in declaration.members) {
            when (member) {
                is Declaration.Struct.Member.Field -> {
                    checkAnnotation(member.typeAnnotation)
                }
            }
        }
    }

    private fun checkExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        checkNameBinding(declaration.binder)
        for (paramType in declaration.paramTypes) {
            checkAnnotation(paramType)
        }
        checkAnnotation(declaration.returnType)
    }

    private fun checkFunctionDef(declaration: Declaration.FunctionDef) {
        checkFunctionSignature(declaration.signature)
        returnTypeStack.push(annotationToType(declaration.signature.returnType))
        checkBlock(declaration.body)
        returnTypeStack.pop()
    }

    private fun annotationToType(type: TypeAnnotation): Type {
        return ctx.typer.annotationToType(type)
    }

    private fun checkNameBinding(name: Binder) {
        // TODO: Check for duplicate bindings
    }

    private fun checkBlock(block: Block) {
        for (member in block.members) {
            checkBlockMember(member)
        }

    }

    private fun checkStatement(statement: Statement): Unit = when(statement) {
        is Statement.Return -> checkReturnStatement(statement)
        is Statement.Val -> checkValStatement(statement)
        is Statement.While -> checkWhileStatement(statement)
        is Statement.If -> checkIfStatement(statement)
        is Statement.LocalAssignment -> TODO()
        is Statement.MemberAssignment -> TODO()
        is Statement.PointerAssignment -> TODO()
        is Statement.Defer -> checkDeferStatement(statement)
        is Statement.Error -> {}
    }

    private fun checkDeferStatement(statement: Statement.Defer) {
        checkBlockMember(statement.blockMember)
    }

    private fun checkBlockMember(member: Block.Member) = when (member) {
        is Block.Member.Expression -> {
            checkExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> checkStatement(member.statement)
    }

    private fun checkWhileStatement(statement: Statement.While) {
        checkExpression(statement.condition, Type.Bool)
        checkBlock(statement.body)
    }

    private fun checkIfStatement(statement: Statement.If) {
        checkExpression(statement.condition, Type.Bool)
        checkBlock(statement.ifTrue)
        statement.ifFalse?.let { checkBlock(it) }
    }

    private fun checkReturnStatement(statement: Statement.Return) {
        val expectedReturnType = returnTypeStack.peek()

        if (statement.value != null) {
            checkExpression(statement.value, expectedReturnType)
        }
    }

    private fun checkValStatement(statement: Statement.Val) {
        checkNameBinding(statement.binder)
        if (statement.typeAnnotation != null) {
            checkAnnotation(statement.typeAnnotation)
            val rhsType = ctx.typer.typeOfExpression(statement.rhs)
            val lhsType = ctx.typer.annotationToType(statement.typeAnnotation)
            if (!ctx.typer.isTypeAssignableTo(
                            source = rhsType,
                            destination = lhsType)) {
                error(statement.rhs, Diagnostic.Kind.TypeNotAssignable(
                        source = rhsType,
                        destination = lhsType
                ))
            }
        }
        checkExpression(statement.rhs)
    }

    private fun checkExpression(expression: Expression, expectedType: Type? = null): Type  {
        exhaustive(when(expression) {
            is Expression.Error -> {}
            is Expression.Var -> checkVarExpression(expression)
            is Expression.Call -> checkCall(expression)
            is Expression.Property -> checkPropertyExpression(expression)
            is Expression.ByteString -> {}
            is Expression.BoolLiteral -> {}
            is Expression.This -> checkThisExpression(expression)
            is Expression.NullPtr -> TODO()
            is Expression.IntLiteral -> {}
            is Expression.Not -> checkNotExpression(expression)
            is Expression.BinaryOperation -> TODO()
            is Expression.SizeOf -> TODO()
            is Expression.AddressOf -> TODO()
            is Expression.AddressOfMut -> checkAddressOfMut(expression)
            is Expression.Deref -> TODO()
            is Expression.PointerCast -> checkPointerCast(expression)
            is Expression.If -> checkIfExpression(expression)
            is Expression.TypeApplication -> TODO()
            is Expression.Match -> TODO()
            is Expression.New -> TODO()
        })
        if (expectedType != null) {
            val exprType = typeOfExpression(expression)
            if (!ctx.typer.isTypeAssignableTo(exprType, expectedType, Variance.INVARIANCE)) {
                error(expression, Diagnostic.Kind.TypeNotAssignable(source = exprType, destination = expectedType))
            }
        }
        return typeOfExpression(expression)
    }

    private fun checkAddressOfMut(expression: Expression.AddressOfMut) {
        // TODO: Check if expression is addressable
    }

    private fun checkPointerCast(expression: Expression.PointerCast) {
        checkAnnotation(expression.toType)
    }

    private fun checkIfExpression(expression: Expression.If): Type {
        checkExpression(expression.condition, Type.Bool)
        val type = checkExpression(expression.trueBranch)
        return checkExpression(expression.falseBranch, type)
    }

    private fun checkNotExpression(expression: Expression.Not) {
        checkExpression(expression.expression, Type.Bool)
    }

    private fun checkThisExpression(expression: Expression.This) {
        val thisBinding = ctx.resolver.resolveThisParam(expression)
        if (thisBinding == null) {
            error(expression, Diagnostic.Kind.UnboundThis)
        }
    }

    private fun checkPropertyExpression(expression: Expression.Property) {
        if (ctx.typer.resolvePropertyBinding(expression) == null) {
            error(expression.property, Diagnostic.Kind.NoSuchProperty(
                    typeOfExpression(expression.lhs),
                    expression.property.name))
        }
    }

    private fun checkCall(expression: Expression.Call) {
        val calleeType = typeOfExpression(expression.callee)
        if (!isTypeCallable(calleeType)) {
            error(expression.callee, Diagnostic.Kind.TypeNotCallable(calleeType))
        }

        val fnTypeComponents = ctx.typer.getFunctionTypeComponents(calleeType)
        val expectedArgTypes = expectedArgTypes(calleeType) ?: emptyList()
        val substitution = ctx.typer.instantiateSubstitution(fnTypeComponents?.typeParams)
        for (index in expression.args.indices) {
            val arg = expression.args[index]
            val expectedType = if (index < expectedArgTypes.size) {
                expectedArgTypes[index].applySubstitution(substitution)
            } else null

            checkExpression(arg.expression, expectedType)
        }

        if (expression.args.size > expectedArgTypes.size) {
            error(expression, Diagnostic.Kind.TooManyArgs(required = expectedArgTypes.size))
        }
        if (expression.args.size < expectedArgTypes.size) {
            error(expression, Diagnostic.Kind.MissingArgs(required = expectedArgTypes.size))
        }

    }

    private fun expectedArgTypes(calleeType: Type): List<Type>? =
            ctx.typer.getFunctionTypeComponents(calleeType)?.from

    private fun isTypeCallable(calleeType: Type): Boolean =
            ctx.typer.getFunctionTypeComponents(calleeType) != null

    private fun typeOfExpression(expression: Expression): Type {
        return ctx.typer.typeOfExpression(expression)
    }

    private fun checkVarExpression(expression: Expression.Var): Type {
        val binding = ctx.resolver.resolve(expression.name)
        if (binding == null) {
            error(expression, Diagnostic.Kind.UnboundVariable)
        }
        return typeOfExpression(expression)
    }

    private fun checkFunctionSignature(signature: FunctionSignature) {
        checkNameBinding(signature.name)
        signature.typeParams?.forEach {
            checkTypeParam(it)
        }
        signature.params.forEach {
            checkParam(it)
        }
        checkAnnotation(signature.returnType)
    }

    private fun checkParam(it: Param) {
        if (it.annotation == null) {
            error(it, Diagnostic.Kind.MissingTypeAnnotation)
        }
        it.annotation?.let { checkAnnotation(it) }
    }

    private fun checkAnnotation(annotation: TypeAnnotation): Unit = when(annotation) {
        is TypeAnnotation.Error -> {}
        is TypeAnnotation.Var -> {
            if (ctx.typer.resolveTypeVariable(annotation) == null) {
                error(annotation, Diagnostic.Kind.UnboundType(annotation.name.name))
            } else {}
        }
        is TypeAnnotation.Ptr -> {
            checkAnnotation(annotation.to)
        }
        is TypeAnnotation.MutPtr -> {
            checkAnnotation(annotation.to)
        }
        is TypeAnnotation.Application -> {
            checkAnnotation(annotation.callee)
            // TODO: Check args
            for (arg in annotation.args) {
                checkAnnotation(arg)
            }
        }
        is TypeAnnotation.Qualified -> {
            val binding = ctx.resolver.resolveQualifiedType(annotation.qualifiedPath)
            if (binding == null) {
                error(annotation.qualifiedPath, Diagnostic.Kind.UnboundTypeName(annotation.qualifiedPath))
            } else {}
        }
        is TypeAnnotation.Function -> {
            annotation.from.forEach {
                checkAnnotation(it)
            }
            checkAnnotation(annotation.to)
        }
        is TypeAnnotation.This -> TODO()
        is TypeAnnotation.Union -> TODO()
    }

    private fun checkTypeParam(param: TypeParam) {

    }

    private fun checkImportAsDeclaration(declaration: Declaration.ImportAs) {
        val sourceFile = ctx.resolveSourceFile(declaration.modulePath)
        if (sourceFile == null) {
            error(declaration.modulePath, Diagnostic.Kind.NoSuchModule)
        }
    }

    private fun error(node: HasLocation, kind: Diagnostic.Kind) {
        ctx.diagnosticReporter.report(node.location, kind)
    }
}