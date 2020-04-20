package hadesc.ir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.logging.logger
import hadesc.resolver.ValueBinding
import hadesc.types.Type

class Desugar(val ctx: Context) {
    private val module = IRModule()
    private val definitions = mutableListOf<IRDefinition>()
    private val loweredSourceFileSet = mutableSetOf<SourcePath>()
    private val builder = IRBuilder()
    private val log = logger()

    fun generate(): IRModule {
        ctx.forEachSourceFile { lowerSourceFile(it) }
        log.debug(module.prettyPrint())
        return module
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

    private val declaredExternDefs = mutableMapOf<SourceLocation, IRExternFunctionDef>()
    private fun lowerExternFunctionDef(declaration: Declaration.ExternFunctionDef): IRExternFunctionDef =
        declaredExternDefs.computeIfAbsent(declaration.location) {
            val def = module.addExternFunctionDef(
                binder = lowerGlobalBinder(declaration.binder),
                externName = declaration.externName.name,
                paramTypes = declaration.paramTypes.map { ctx.checker.annotationToType(it) }
            )
            definitions.add(def)
            def
        }

    private val addedStructDefs = mutableMapOf<SourceLocation, IRStructDef>()
    private fun lowerStructDeclaration(declaration: Declaration.Struct): IRStructDef {
        val existing = addedStructDefs[declaration.location]
        if (existing != null) {
            return existing
        }
        val fields = declaration.members.map {
            when (it) {
                Declaration.Struct.Member.Error -> requireUnreachable()
                is Declaration.Struct.Member.Field -> it.binder.identifier.name to ctx.checker.annotationToType(it.typeAnnotation)
            }
        }.toMap()
        val def = module.addStructDef(
            ctx.checker.typeOfStructConstructor(declaration),
            ctx.checker.typeOfStructInstance(declaration),
            lowerBinderName(declaration.binder),
            typeParams = declaration.typeParams?.map { lowerTypeParam(it) },
            fields = fields
        )
        definitions.add(def)
        addedStructDefs[declaration.location] = def
        return def
    }

    private val declaredFunctionDefs = mutableMapOf<SourceLocation, IRFunctionDef>()

    private fun getFunctionDef(def: Declaration.FunctionDef): IRFunctionDef {
        return declaredFunctionDefs.computeIfAbsent(def.location) {
            val binder = lowerGlobalBinder(def.name)
            val function = module.addGlobalFunctionDef(
                binder = binder,
                typeParams = def.typeParams?.map { lowerTypeParam(it) },
                params = def.params.map { lowerParam(it) },
                body = IRBlock()
            )
            definitions.add(function)
            function
        }

    }

    private fun lowerTypeParam(typeParam: TypeParam): IRTypeBinder {
        return IRTypeBinder(typeParam.binder.identifier.name, typeParam.location)
    }

    private fun lowerGlobalFunctionDef(def: Declaration.FunctionDef): IRFunctionDef {
        val function = getFunctionDef(def)
        function.body = lowerBlock(def.body)
        val ty = function.type
        require(ty is Type.Function)
        builder.withinBlock(function.body) {
            if (ctx.checker.isTypeEqual(ty.to, Type.Void)) {
                builder.buildRetVoid()
            }
        }
        return function
    }

    private fun lowerParam(param: Param): IRParam {
        return IRParam(lowerParamBinder(param.binder), param.location)
    }

    private fun lowerBlock(body: Block): IRBlock {
        val block = IRBlock()
        builder.withinBlock(block) {
            for (member in body.members) {
                lowerBlockMember(member)
            }
        }
        return block
    }

    private fun lowerBlockMember(member: Block.Member) = when (member) {
        is Block.Member.Expression -> {
            lowerExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> {
            lowerStatement(member.statement)
            Unit
        }
    }

    private fun lowerExpression(expression: Expression): IRValue = when (expression) {
        is Expression.Error -> requireUnreachable()
        is Expression.Var -> lowerVar(expression)
        is Expression.Call -> lowerCall(expression)
        is Expression.Property -> lowerProperty(expression)
        is Expression.ByteString -> lowerByteString(expression)
        is Expression.BoolLiteral -> lowerBoolLiteral(expression)
    }

    private fun lowerBoolLiteral(expression: Expression.BoolLiteral): IRValue {
        val ty = typeOfExpression(expression)
        return builder.buildConstBool(ty, expression.location, expression.value)
    }

    private fun lowerByteString(expression: Expression.ByteString): IRValue {
        return builder.buildByteString(
            ctx.checker.typeOfExpression(expression),
            expression.location,
            expression.bytes
        )
    }

    private fun lowerProperty(expression: Expression.Property): IRValue =
        when (val binding = ctx.resolver.resolveModuleProperty(expression)) {
            null -> lowerRuntimePropertyAccess(expression)
            else -> lowerBindingRef(typeOfExpression(expression), expression, binding)
        }

    private fun lowerRuntimePropertyAccess(expression: Expression.Property): IRValue {
        val lhs = lowerExpression(expression.lhs)
        val lhsType = lhs.type
        require(lhsType is Type.Struct)
        val rhsType = lhsType.memberTypes[expression.property.name]
        requireNotNull(rhsType)
        val index = lhsType.indexOf(expression.property.name.text)
        require(index > -1)
        return builder.buildGetStructField(
            rhsType,
            expression.location,
            lhs,
            expression.property.name,
            index
        )
    }

    private fun lowerVar(variable: Expression.Var): IRValue {
        return lowerBindingRef(ctx.checker.typeOfExpression(variable), variable, ctx.resolver.resolve(variable.name))
    }

    private fun lowerBindingRef(ty: Type, node: HasLocation, binding: ValueBinding?): IRValue {
        val irBinding: IRBinding = when (binding) {
            null -> requireUnreachable()
            is ValueBinding.GlobalFunction -> {
                val def = getFunctionDef(binding.declaration)
                IRBinding.FunctionDef(def)
            }
            is ValueBinding.ExternFunction -> {
                val def = lowerExternFunctionDef(binding.declaration)
                IRBinding.ExternFunctionDef(def)
            }
            is ValueBinding.FunctionParam -> {
                val index = binding.index
                assert(index > -1)
                IRBinding.ParamRef(getFunctionDef(binding.declaration), index)
            }
            is ValueBinding.ValBinding -> {
                val statement = getValBinding(binding.statement)
                IRBinding.Local(statement.binder.name)
            }
            is ValueBinding.Struct -> {
                val structDecl = lowerStructDeclaration(binding.declaration)
                IRBinding.StructDef(structDecl)
            }
        }
        return builder.buildVariable(ty, node.location, irBinding)

    }

    private fun lowerCall(expression: Expression.Call): IRValue {
        val callee = lowerExpression(expression.callee)
        val args = expression.args.map { lowerExpression(it.expression) }
        val type = typeOfExpression(expression)
        return builder.buildCall(
            type,
            expression.location,
            callee,
            typeArgs = ctx.checker.getTypeArgs(expression),
            args = args,
            name = ctx.makeUniqueName()
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
        return builder.buildReturn(lowerExpression(statement.value))
    }

    private val loweredValStatements = mutableMapOf<SourceLocation, IRValStatement>()
    private fun lowerValStatement(statement: Statement.Val): IRValStatement {
        val ir = builder.buildValStatement(lowerLocalBinder(statement.binder), lowerExpression(statement.rhs))
        loweredValStatements[statement.location] = ir
        return ir
    }

    private fun getValBinding(statement: Statement.Val): IRValStatement {
        return requireNotNull(loweredValStatements[statement.location])
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

