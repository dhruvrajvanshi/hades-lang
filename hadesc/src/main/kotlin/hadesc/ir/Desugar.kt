package hadesc.ir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.resolver.ValueBinding
import hadesc.types.Type

class Desugar(val ctx: Context) {
    private val definitions = mutableListOf<IRDefinition>()
    private val loweredSourceFileSet = mutableSetOf<SourcePath>()

    fun generate(): IRModule {
        ctx.forEachSourceFile { lowerSourceFile(it) }
        return IRModule(definitions)
    }

    private fun lowerSourceFile(sourceFile: SourceFile) {
        // this is only required because there may be circular
        // dependencies between source files
        if (loweredSourceFileSet.contains(sourceFile.location.file)) {
            return
        }
        loweredSourceFileSet.add(sourceFile.location.file)
        lowerSourceFileHelper(sourceFile)
    }

    private fun lowerSourceFileHelper(sourceFile: SourceFile) {
        for (declaration in sourceFile.declarations) {
            lowerDeclaration(declaration)
        }
    }

    private fun lowerDeclaration(declaration: Declaration) = when (declaration) {
        is Declaration.Error -> requireUnreachable()
        is Declaration.ImportAs -> Unit
        is Declaration.FunctionDef -> {
            lowerGlobalFunctionDef(declaration)
            Unit
        }
        is Declaration.ExternFunctionDef -> {
            lowerExternFunctionDef(declaration)
            Unit
        }
        is Declaration.Struct -> {
            lowerStructDeclaration(declaration)
            Unit
        }
    }

    private fun lowerExternFunctionDef(declaration: Declaration.ExternFunctionDef): IRExternFunctionDef {
        val def = IRExternFunctionDef(
            binder = lowerGlobalBinder(declaration.binder),
            externName = declaration.externName.name,
            paramTypes = declaration.paramTypes.map { ctx.checker.annotationToType(it) }
        )
        definitions.add(def)
        return def
    }

    private fun lowerStructDeclaration(declaration: Declaration.Struct): IRStructDef {
        val fields = declaration.members.map {
            when (it) {
                Declaration.Struct.Member.Error -> requireUnreachable()
                is Declaration.Struct.Member.Field -> it.binder.identifier.name to ctx.checker.annotationToType(it.typeAnnotation)
            }
        }.toMap()
        val def = IRStructDef(
            ctx.checker.typeOfStructConstructor(declaration),
            ctx.checker.typeOfStructInstance(declaration),
            lowerBinderName(declaration.binder),
            // TODO: Handle generic structs
            typeParams = listOf(),
            fields = fields
        )
        definitions.add(def)
        return def
    }

    private val declaredFunctionDefs = mutableMapOf<SourceLocation, IRFunctionDef>()

    private fun getFunctionDef(def: Declaration.FunctionDef): IRFunctionDef {
        return declaredFunctionDefs.computeIfAbsent(def.location) {
            val binder = lowerGlobalBinder(def.name)
            val function = IRFunctionDef(
                binder = binder,
                typeParams = listOf(),
                params = def.params.map { lowerParam(it) },
                body = IRBlock(mutableListOf())
            )
            definitions.add(function)
            function
        }

    }

    private fun lowerGlobalFunctionDef(def: Declaration.FunctionDef): IRFunctionDef {
        require(def.typeParams.isEmpty())
        val function = getFunctionDef(def)
        function.body = lowerBlock(def.body)
        val ty = function.type
        require(ty is Type.Function)
        if (ctx.checker.isTypeEqual(ty.to, Type.Void)) {
            function.body.statements.add(IRReturnVoidStatement)
        }
        return function
    }

    private fun lowerParam(param: Param): IRParam {
        return IRParam(lowerParamBinder(param.binder))
    }

    private fun lowerBlock(body: Block): IRBlock {
        val statements = mutableListOf<IRStatement>()
        for (member in body.members) {
            statements.add(lowerBlockMember(member))
        }
        return IRBlock(statements)
    }

    private fun lowerBlockMember(member: Block.Member): IRStatement = when (member) {
        is Block.Member.Expression -> {
            lowerExpression(member.expression)
        }
        is Block.Member.Statement -> lowerStatement(member.statement)
    }

    private fun lowerExpression(expression: Expression): IRExpression = when (expression) {
        is Expression.Error -> requireUnreachable()
        is Expression.Var -> lowerVar(expression)
        is Expression.Call -> lowerCall(expression)
        is Expression.Property -> lowerProperty(expression)
        is Expression.ByteString -> lowerByteString(expression)
        is Expression.BoolLiteral -> lowerBoolLiteral(expression)
    }

    private fun lowerBoolLiteral(expression: Expression.BoolLiteral): IRExpression {
        return IRBool(typeOfExpression(expression), expression.location, expression.value)
    }

    private fun lowerByteString(expression: Expression.ByteString): IRExpression {
        return IRByteString(
            ctx.checker.typeOfExpression(expression),
            expression.location,
            expression.bytes
        )
    }

    private fun lowerProperty(expression: Expression.Property): IRExpression = when (expression.lhs) {
        is Expression.Var -> {
            when (val binding = ctx.resolver.getBinding(expression.lhs.name)) {
                is ValueBinding.ImportAs -> {
                    val sourceFile = ctx.resolveSourceFile(binding.declaration.modulePath)
                    val propertyBinding = requireNotNull(
                        ctx.resolver.findInSourceFile(
                            expression.property, sourceFile
                        )
                    )
                    val ty = ctx.checker.typeOfBinding(propertyBinding)
                    when (propertyBinding) {
                        is ValueBinding.ImportAs -> TODO()
                        is ValueBinding.GlobalFunction -> {
                            IRVariable(
                                ty,
                                expression.property.location,
                                IRBinding.FunctionDef(getFunctionDef(propertyBinding.declaration))
                            )
                        }
                        is ValueBinding.ExternFunction -> {
                            IRVariable(
                                ty,
                                expression.property.location,
                                IRBinding.ExternFunctionDef(lowerExternFunctionDef(propertyBinding.declaration))
                            )
                        }
                        is ValueBinding.FunctionParam -> requireUnreachable() // a property access can't refer to a param
                        is ValueBinding.ValBinding -> requireUnreachable()
                        is ValueBinding.Struct -> IRVariable(
                            ty,
                            expression.property.location,
                            IRBinding.StructDef(lowerStructDeclaration(propertyBinding.declaration))
                        )
                    }
                }
                else -> lowerRuntimePropertyAccess(expression)
            }
        }
        else -> {
            lowerRuntimePropertyAccess(expression)
        }
    }

    private fun lowerRuntimePropertyAccess(expression: Expression.Property): IRExpression {
        val lhs = lowerExpression(expression.lhs)
        val lhsType = lhs.type
        require(lhsType is Type.Struct)
        val rhsType = lhsType.memberTypes[expression.property.name]
        requireNotNull(rhsType)
        val index = lhsType.indexOf(expression.property.name.text)
        require(index > -1)
        return IRGetStructField(
            rhsType,
            expression.location,
            lhs,
            expression.property.name,
            index
        )
    }

    private fun lowerVar(variable: Expression.Var): IRExpression {
        val irBinding: IRBinding = when (val binding = ctx.resolver.getBinding(variable.name)) {
            is ValueBinding.GlobalFunction -> {
                val def = getFunctionDef(binding.declaration)
                IRBinding.FunctionDef(def)
            }
            is ValueBinding.ExternFunction -> {
                val def = lowerExternFunctionDef(binding.declaration)
                IRBinding.ExternFunctionDef(def)
            }
            is ValueBinding.FunctionParam -> {
                val index = binding.declaration.params.indexOfFirst {
                    it.binder.identifier.name == variable.name.name
                }
                assert(index > -1)
                IRBinding.ParamRef(getFunctionDef(binding.declaration), index)
            }
            is ValueBinding.ImportAs -> requireUnreachable()
            is ValueBinding.ValBinding -> {
                val statement = lowerValStatement(binding.statement)
                IRBinding.ValStatement(statement)
            }
            is ValueBinding.Struct -> {
                val structDecl = lowerStructDeclaration(binding.declaration)
                IRBinding.StructDef(structDecl)
            }
        }
        val ty = ctx.checker.typeOfExpression(variable)
        return IRVariable(ty, variable.location, irBinding)
    }

    private fun lowerCall(expression: Expression.Call): IRExpression {
        val callee = lowerExpression(expression.callee)
        val args = expression.args.map { lowerExpression(it.expression) }
        val type = typeOfExpression(expression)
        // TODO: Handle generic calls
        return IRCallExpression(
            type,
            expression.location,
            callee,
            typeArgs = null,
            args = args
        )
    }

    private fun typeOfExpression(expression: Expression): Type {
        return ctx.checker.typeOfExpression(expression)
    }

    private fun lowerStatement(statement: Statement): IRStatement = when (statement) {
        is Statement.Return -> lowerReturnStatement(statement)
        is Statement.Val -> lowerValStatement(statement)
        is Statement.Error -> TODO()
    }

    private fun lowerReturnStatement(statement: Statement.Return): IRStatement {
        return IRReturnStatement(lowerExpression(statement.value))
    }

    private fun lowerValStatement(statement: Statement.Val): IRValStatement {
        return IRValStatement(lowerLocalBinder(statement.binder), lowerExpression(statement.rhs))
    }

    private fun lowerGlobalBinder(name: Binder): IRBinder {
        val sourceFile = ctx.getSourceFileOf(name)
        val ty = ctx.checker.typeOfBinder(name)
        return IRBinder(ctx.makeName(sourceFile.moduleName.append(name.identifier.name).mangle()), ty)
    }

    private fun lowerLocalBinder(name: Binder): IRBinder {
        val sourceFile = ctx.getSourceFileOf(name)
        val ty = ctx.checker.typeOfBinder(name)
        return IRBinder(ctx.makeName(sourceFile.moduleName.append(name.identifier.name).mangle()), ty)
    }

    private fun lowerBinderName(name: Binder): Name {
        val sourceFile = ctx.getSourceFileOf(name)
        return ctx.makeName(sourceFile.moduleName.append(name.identifier.name).mangle())
    }

    private fun lowerParamBinder(name: Binder): IRBinder {
        return lowerLocalBinder(name)
    }
}

