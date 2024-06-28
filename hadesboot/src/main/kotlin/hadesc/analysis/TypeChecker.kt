package hadesc.analysis

import hadesc.Name
import hadesc.ast.*
import hadesc.diagnostics.Diagnostic
import hadesc.diagnostics.DiagnosticReporter
import hadesc.hir.TypeVisitor
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.resolver.Binding
import hadesc.resolver.NewResolver
import hadesc.resolver.TypeBinding
import hadesc.types.Type
import hadesc.types.ptr

fun typecheck(sourceFiles: List<SourceFile>, diagnosticReporter: DiagnosticReporter, resolver: NewResolver) {
    TypeChecker(diagnosticReporter, sourceFiles, resolver).check()
}

class TypeChecker(
    private val diagnostic: DiagnosticReporter,
    private val program: List<SourceFile>,
    private val resolver: NewResolver,
) {
    fun check() {
        for (file in program) {
            visitSourceFile(file)
        }
    }

    private fun visitSourceFile(file: SourceFile) {
        for (decl in file.declarations) {
            visitDeclaration(decl)
        }
    }

    private fun visitDeclaration(decl: Declaration): Unit = when (decl) {
        is Declaration.Error -> {}
        is Declaration.ExternConst -> visitExternConst(decl)
        is Declaration.ExternFunctionDef -> visitExternFunctionDef(decl)
        is Declaration.FunctionDef -> visitFunctionDef(decl)
        is Declaration.ImportMembers -> visitImportMembers(decl)
        is Declaration.TypeAlias -> visitTypeAlias(decl)
        is Declaration.Struct -> visitStructDecl(decl)
        is Declaration.ConstDefinition -> visitConstDefinition(decl)
        is Declaration.Enum,
        is Declaration.ExtensionDef,
        is Declaration.ImplementationDef,
        is Declaration.ImportAs,
        is Declaration.TraitDef,
        -> todo(decl)
    }

    private fun visitConstDefinition(decl: Declaration.ConstDefinition) {
        checkTopLevelExpressionBinding(decl.name)
        val expectedType = decl.annotation?.lower()
        if (expectedType != null) {
            checkExpressionType(decl.initializer, expectedType)
        } else {
            inferExpression(decl.initializer)
        }

    }

    private fun visitStructDecl(decl: Declaration.Struct) {
        checkTopLevelExpressionBinding(decl.binder)
        val visitedFields = mutableSetOf<Name>()
        visitTypeParams(decl.typeParams)
        if (decl.typeParams != null) {
            todo(decl, "The new typechecker doesn't handle generic structs yet.")
        }
        for (member in decl.members) {
            when (member) {
                is Declaration.Struct.Member.Field -> {
                    if (member.binder.name in visitedFields) {
                        diagnostic.report(member.binder.location, "Duplicate field name `${member.binder.name}`")
                    } else {
                        visitedFields.add(member.binder.name)
                    }
                    member.typeAnnotation.lower()
                }
            }
        }
    }

    private fun visitTypeAlias(decl: Declaration.TypeAlias) {
        if (decl.typeParams != null) {
            todo(decl, "The new typechecker doesn't handle type parameters yet.")
        }
        checkTopLevelTypeBinding(decl.name)
        visitTypeParams(decl.typeParams)
        decl.rhs.lower()
    }

    private fun visitImportMembers(decl: Declaration.ImportMembers) {
        val sourceFile = resolver.getSourceFile(decl.modulePath)
        if (sourceFile == null) {
            diagnostic.report(decl.modulePath.location, "Can not find module `${decl.modulePath}`")
            return
        }

        for (name in decl.names) {
            val typeBinding = resolver.findTypeInSourceFile(name.identifier, sourceFile)
            val trait = resolver.findTraitInSourceFile(name.identifier, sourceFile)
            val value = resolver.findInSourceFile(name.identifier.name, sourceFile)

            if (typeBinding == null && trait == null && value == null) {
                diagnostic.report(
                    name.location,
                    "Can not find `${name.identifier.name.text}` in module `${decl.modulePath}`"
                )
            }
        }

    }

    private fun visitFunctionDef(decl: Declaration.FunctionDef) {
        checkTopLevelExpressionBinding(decl.name)
        visitTypeParams(decl.typeParams)
        val visitedParams = mutableSetOf<Name>()
        for (param in decl.params) {
            if (param.binder.name in visitedParams) {
                diagnostic.report(param.binder.location, "Duplicate parameter name `${param.binder.name}`")
            } else {
                visitedParams.add(param.binder.name)
            }
            param.annotation?.lower()
        }
        val returnType = decl.signature.returnType.lower()

        if (returnType.containsClosures()) {
            diagnostic.report(decl.signature.returnType.location, "Function return type can not contain closures")
        }

        visitFunctionBody(decl.body)
    }

    private fun Type.containsClosures(): Boolean {
        var containsClosures = false
        val visitor = object : TypeVisitor {
            override fun visitType(type: Type) {
                if (type is Type.Closure) {
                    containsClosures = true
                }
                super.visitType(type)
            }
        }
        visitor.visitType(this)
        return containsClosures
    }

    private fun visitFunctionBody(body: Block) {
        visitBlock(body)
    }

    private fun visitBlock(block: Block) {
        for (member in block.members) {
            visitBlockMember(member)
        }
    }

    private fun visitBlockMember(member: Block.Member): Unit = when (member) {
        is Block.Member.Expression -> {
            inferExpression(member.expression)
            Unit
        }

        is Block.Member.Statement -> {
            visitStatement(member.statement)
        }
    }

    private fun inferExpression(expression: Expression, constraint: ValueConstraint = ValueConstraint.None): Type =
        when (expression) {
            is Expression.AddressOf -> todo(expression)
            is Expression.AddressOfMut -> todo(expression)
            is Expression.AlignOf -> todo(expression)
            is Expression.As -> todo(expression)
            is Expression.BinaryOperation -> todo(expression)
            is Expression.BlockExpression -> todo(expression)
            is Expression.BoolLiteral -> Type.Bool
            is Expression.ByteCharLiteral -> Type.u8
            is Expression.ByteString -> Type.u8.ptr()
            is Expression.Call -> inferCall(expression)
            is Expression.Closure -> todo(expression)
            is Expression.Deref -> todo(expression)
            is Expression.Error -> todo(expression)
            is Expression.FloatLiteral -> todo(expression)
            is Expression.If -> todo(expression)
            is Expression.IntLiteral -> inferIntLiteral(expression, constraint)
            is Expression.Intrinsic -> todo(expression)
            is Expression.Match -> todo(expression)
            is Expression.Move -> todo(expression)
            is Expression.Not -> todo(expression)
            is Expression.NullPtr -> todo(expression)
            is Expression.PointerCast -> todo(expression)
            is Expression.Property -> todo(expression)
            is Expression.SizeOf -> todo(expression)
            is Expression.This -> todo(expression)
            is Expression.UnaryMinus -> todo(expression)
            is Expression.Uninitialized -> todo(expression)
            is Expression.Var -> {
                inferVar(expression, constraint)
            }

            is Expression.TypeApplication -> todo(expression)
        }

    private fun inferIntLiteral(expression: Expression.IntLiteral, constraint: ValueConstraint): Type =
        when (constraint) {
            is ValueConstraint.HasType -> when (constraint.type) {
                is Type.Integral -> constraint.type
                is Type.Size -> constraint.type
                else -> reportAndMakeErrorType(expression.location, "Expected an integral type")
            }

            else -> Type.i64
        }

    private fun inferVar(
        expression: Expression.Var,
        @Suppress("UNUSED_PARAMETER")
        // Will be used soon when we start to pass down constraints
        // that aid with inference; e.g. call arguments
        constraint: ValueConstraint
    ): Type = when (resolver.resolve(expression.name)) {
        null ->
            reportAndMakeErrorType(
                expression.location,
                "Can not find `${expression.name.name.text}` in this scope"
            )

        is Binding.ClosureParam -> todoType(expression)
        is Binding.Enum -> todoType(expression)
        is Binding.ExternConst -> todoType(expression)
        is Binding.ExternFunction -> todoType(expression)
        is Binding.FunctionParam -> todoType(expression)
        is Binding.GlobalConst -> todoType(expression)
        is Binding.GlobalFunction -> todoType(expression)
        is Binding.MatchArmEnumCaseArg -> todoType(expression)
        is Binding.ValBinding -> todoType(expression)
        is Binding.Struct -> todoType(expression)
    }

    private fun inferCall(expression: Expression.Call): Type {
        return when (val callee = resolveCallee(expression)) {
            is Callee.ExternFunction -> {
                val expectedParams = callee.definition.paramTypes.map { it.lower() }
                val expectedArguments = expression.args.take(expectedParams.size)

                for ((expectedParam, argument) in expectedParams.zip(expectedArguments)) {
                    checkExpressionType(argument.expression, expectedParam)
                }
                val extraArguments = expression.args.drop(expectedParams.size)
                for (extraArgument in extraArguments) {
                    diagnostic.report(
                        extraArgument.location,
                        "Unexpected argument; ${callee.definition.binder.name.text} expects ${expectedArguments.size} argument(s)"
                    )
                }
                if (expectedParams.size > expression.args.size) {
                    diagnostic.report(
                        expression.location,
                        "Missing arguments; ${callee.definition.binder.name.text} expects ${expectedArguments.size} argument(s)"
                    )
                }
                callee.definition.returnType.lower()
            }

            is Callee.Function -> {
                val expectedParams = callee.definition.params.map { it.annotation?.lower() ?: Type.Error(it.location, "Type annotation not provided") }
                val expectedArguments = expression.args.take(expectedParams.size)

                for ((expectedParam, argument) in expectedParams.zip(expectedArguments)) {
                    checkExpressionType(argument.expression, expectedParam)
                }
                val extraArguments = expression.args.drop(expectedParams.size)
                for (extraArgument in extraArguments) {
                    diagnostic.report(
                        extraArgument.location,
                        "Unexpected argument; ${callee.definition.name.name.text} expects ${expectedArguments.size} argument(s)"
                    )
                }
                if (expectedParams.size > expression.args.size) {
                    diagnostic.report(
                        expression.location,
                        "Missing arguments; ${callee.definition.name.name.text} expects ${expectedArguments.size} argument(s)"
                    )
                }
                callee.definition.signature.returnType.lower()
            }
            null -> reportAndMakeErrorType(expression.location, "The callee could not be resolved.")
        }
    }

    private fun checkExpressionType(expression: Expression, expectedParam: Type) {
        val inferredType = inferExpression(expression, ValueConstraint.HasType(expectedParam))
        if (!TypeAnalyzer().isTypeAssignableTo(source = inferredType, destination = expectedParam)) {
            diagnostic.report(expression.location, "Expected type $expectedParam but got $inferredType")
        }
    }


    private fun resolveCallee(expression: Expression.Call): Callee? = when (expression.callee) {
        is Expression.Var -> {
            when (val binding = resolver.resolve(expression.callee.name)) {
                is Binding.GlobalFunction -> Callee.Function(binding.declaration)
                is Binding.ExternFunction -> Callee.ExternFunction(binding.declaration)
                else -> null
            }
        }

        else -> {
            todo(expression, "resolveCallee doesn't handle ${expression::class.simpleName} yet.")
            null
        }
    }


    private fun visitStatement(statement: Statement) {
        todo(statement)
    }

    private fun visitTypeParams(typeParams: List<TypeParam>?) {
        if (typeParams == null) return
        for (typeParam in typeParams) {
            visitTypeParam(typeParam)
        }
    }

    private fun visitTypeParam(typeParam: TypeParam) {
        todo(typeParam)
    }

    private fun visitExternFunctionDef(decl: Declaration.ExternFunctionDef) {
        checkTopLevelExpressionBinding(decl.binder)
        val paramTypes = decl.paramTypes.map { it.lower() }
        val returnType = decl.returnType.lower()

        for ((paramType, annotation) in paramTypes.zip(decl.paramTypes)) {
            if (!paramType.isExternSafe()) {
                diagnostic.report(annotation.location, "This type is not allowed in extern functions")
            }
        }
        if (!returnType.isExternSafe()) {
            diagnostic.report(decl.returnType.location, "This type is not allowed in extern functions")
        }
    }

    /**
     * Types that can be passed to or returned from extern functions
     */
    private fun Type.isExternSafe(): Boolean {
        return when (this) {
            is Type.Constructor,
            is Type.Integral,
            is Type.UntaggedUnion,
            is Type.Size,
            Type.Void,
            Type.Bool,
            is Type.FloatingPoint -> true

            is Type.Ptr -> to.isExternSafe()
            is Type.Application -> false
            is Type.Closure -> false
            is Type.FunctionPtr -> from.all { it.isExternSafe() } && to.isExternSafe()
            is Type.GenericInstance -> false
            is Type.Param -> false
            is Type.TypeFunction -> false
            is Type.AssociatedTypeRef -> false
            is Type.Ref -> false
            is Type.Select -> false
            is Type.Array -> itemType.isExternSafe()
            is Type.Error -> true
        }
    }

    private fun visitExternConst(decl: Declaration.ExternConst) {
        checkTopLevelExpressionBinding(decl.name)
        decl.type.lower()
    }

    private val loweredTypes = nodeMapOf<TypeAnnotation, Type>()
    private fun TypeAnnotation.lower(): Type = loweredTypes.getOrPut(this) {
        when (this) {
            is TypeAnnotation.Application -> todo(this)
            is TypeAnnotation.Array -> todo(this)
            is TypeAnnotation.Closure -> todo(this)
            is TypeAnnotation.Error -> todo(this)
            is TypeAnnotation.FunctionPtr -> todo(this)
            is TypeAnnotation.MutPtr -> Type.Ptr(to.lower(), isMutable = true)
            is TypeAnnotation.Ptr -> Type.Ptr(to.lower(), isMutable = false)
            is TypeAnnotation.Qualified -> todo(this)
            is TypeAnnotation.Select -> todo(this)
            is TypeAnnotation.Union -> todo(this)
            is TypeAnnotation.Var -> {
                when (val binding = resolver.resolveTypeVariable(name)) {
                    is TypeBinding.Builtin -> binding.type
                    is TypeBinding.AssociatedType -> todo(this)
                    is TypeBinding.Enum -> todo(this)
                    is TypeBinding.Struct -> {
                        if (binding.declaration.typeParams != null) {
                            todo(this, "The new typechecker doesn't handle type parameters yet.")
                        }
                        Type.Constructor(resolver.qualifiedName((binding.declaration.binder)))
                    }

                    is TypeBinding.Trait -> todo(this)
                    is TypeBinding.TypeAlias -> {
                        if (binding.declaration.typeParams != null) {
                            todo(this, "The new typechecker doesn't handle type parameters yet.")
                        }
                        binding.declaration.rhs.lower()
                    }

                    is TypeBinding.TypeParam -> todo(this)
                    null -> {
                        reportAndMakeErrorType(location, "Can not find `${name.name.text}` in this scope")
                    }
                }
            }
        }
    }

    private fun reportAndMakeErrorType(l: HasLocation, message: String): Type.Error {
        diagnostic.report(l.location, message)
        return Type.Error(l.location, message)
    }

    private fun todoType(l: HasLocation, message: String = "The new typechecker doesn't support this node yet."): Type {
        diagnostic.report(l.location, message)
        return Type.Error(l.location, message)
    }

    private fun todo(decl: Declaration) {
        diagnostic.report(decl.startLoc, "The new typechecker doesn't support this declaration type yet.")
    }

    private fun todo(loc: HasLocation, message: String = "The new typechecker doesn't support this node type yet.") {
        diagnostic.report(loc.location, message)
    }

    private fun todo(type: TypeAnnotation): Type {
        val message = "The new typechecker doesn't support this node type yet."
        diagnostic.report(type.location, message)
        return Type.Error(type.location, message)
    }

    private fun todo(expr: Expression): Type {
        val message = "The new typechecker doesn't support this expression type yet."
        diagnostic.report(expr.location, message)
        return Type.Error(expr.location, message)
    }

    private val topLevelExpressionBindingsByFile = mutableMapOf<SourcePath, MutableMap<Name, Binder>>()
    private fun checkTopLevelExpressionBinding(binder: Binder) {
        val topLevelExpressionBindings =
            topLevelExpressionBindingsByFile.getOrPut(binder.location.file) { mutableMapOf() }
        if (binder.name in topLevelExpressionBindings) {
            diagnostic.report(binder.location, "Duplicate top-level binding ${binder.name.text}")
        } else {
            topLevelExpressionBindings[binder.name] = binder
        }
    }

    private val topLevelTypeBindingsBySourcePath = mutableMapOf<SourcePath, MutableMap<Name, Binder>>()
    private fun checkTopLevelTypeBinding(binder: Binder) {
        val topLevelTypeBindings = topLevelTypeBindingsBySourcePath.getOrPut(binder.location.file) { mutableMapOf() }
        binder.location.file
        if (binder.name in topLevelTypeBindings) {
            diagnostic.report(binder.location, "Duplicate top-level type definition ${binder.name.text}")
        } else {
            topLevelTypeBindings[binder.name] = binder
        }
    }

}

private fun <T : HasLocation, V> nodeMapOf() = NodeMap<T, V>()
private class NodeMap<T : HasLocation, V> {
    private val map = mutableMapOf<SourceLocation, V>()

    fun getOrPut(key: T, compute: () -> V): V {
        val existing = map[key.location]
        if (existing != null) {
            return existing
        }
        val value = compute()
        map[key.location] = value
        return value
    }

    operator fun get(key: T): V? {
        return map[key.location]
    }

    operator fun set(key: T, value: V) {
        map[key.location] = value
    }
}

sealed interface ValueConstraint {
    data object None : ValueConstraint
    data class HasType(val type: Type) : ValueConstraint
    data object IsCallable : ValueConstraint
}

sealed interface Callee {
    data class Function(val definition: Declaration.FunctionDef) : Callee

    /**
     * Can probably be merged with [Function]
     */
    data class ExternFunction(val definition: Declaration.ExternFunctionDef) : Callee
    data class Closure(val closure: Expression.Closure)
}