package hadesc.ir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.resolver.TypeBinding
import hadesc.resolver.ValueBinding
import hadesc.types.Type

class Desugar(private val ctx: Context) {
    private val module = IRModule()
    private val definitions = mutableListOf<IRDefinition>()
    private val loweredSourceFileSet = mutableSetOf<SourcePath>()
    private val builder = IRBuilder()
    private var currentFunction: IRFunctionDef? = null

    fun generate(): IRModule {
        ctx.forEachSourceFile { lowerSourceFile(it) }
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
            val (name, type) = lowerGlobalBinder(declaration.binder)
            require(type is Type.Function)
            val def = module.addExternFunctionDef(
                name,
                type,
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
        val (name, type) = lowerGlobalBinder(declaration.binder)
        val def = module.addStructDef(
            type as Type.Function,
            ctx.checker.typeOfStructInstance(declaration),
            name,
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
            require(def.thisParam == null) { TODO() }
            val (name, type) = lowerGlobalBinder(def.name)
            require(type is Type.Function)
            val function = module.addGlobalFunctionDef(
                name,
                type,
                typeParams = def.typeParams?.map { lowerTypeParam(it) },
                params = def.params.mapIndexed { index, it -> lowerParam(it, name, index) },
                entryBlock = IRBlock()
            )
            currentFunction = function
            definitions.add(function)
            function
        }

    }

    private fun lowerTypeParam(typeParam: TypeParam): IRTypeParam {
        return IRTypeParam(IRLocalName(typeParam.binder.identifier.name), typeParam.location)
    }

    private fun lowerGlobalFunctionDef(def: Declaration.FunctionDef): IRFunctionDef {
        val function = getFunctionDef(def)
        function.entryBlock = lowerBlock(def.body)
        val ty = function.type

        if (ctx.checker.isTypeEqual(ty.to, Type.Void)) {
            builder.buildRetVoid()
        }
        return function
    }

    private fun lowerParam(param: Param, functionName: IRGlobalName, index: Int): IRParam {
        val (name, type) = lowerParamBinder(param.binder)
        return IRParam(name, type, param.location, functionName, index)
    }

    private fun lowerBlock(body: Block, block: IRBlock = IRBlock(IRLocalName(Name("entry")))): IRBlock {
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

    private fun lowerExpression(expression: Expression): IRValue {
        val lowered = when (expression) {
            is Expression.Error -> requireUnreachable()
            is Expression.Var -> lowerVar(expression)
            is Expression.Call -> lowerCall(expression)
            is Expression.Property -> lowerProperty(expression)
            is Expression.ByteString -> lowerByteString(expression)
            is Expression.BoolLiteral -> lowerBoolLiteral(expression)
            is Expression.This -> TODO()
            is Expression.NullPtr -> IRNullPtr(typeOfExpression(expression), expression.location)
            is Expression.IntLiteral -> IRCIntConstant(
                typeOfExpression(expression),
                expression.location,
                expression.value
            )
            is Expression.Not -> {
                val ty = typeOfExpression(expression)
                val name = IRLocalName(ctx.makeUniqueName())
                builder.buildNot(ty, expression.location, name, lowerExpression(expression.expression))
                builder.buildVariable(ty, expression.location, name)
            }
        }
        assert(lowered.type == typeOfExpression(expression)) {
            "Type of lowered expression at ${expression.location} is not same as unlowered expression: " +
                    "lowered type: ${lowered.type.prettyPrint()}\n" +
                    "original type: ${typeOfExpression(expression).prettyPrint()}"
        }
        return lowered
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
        require(lhsType is Type.Struct || lhsType is Type.Application) {
            TODO()
        }

        val members: Map<Name, Type> = if (lhsType is Type.Struct) {
            lhsType.memberTypes
        } else if (lhsType is Type.Application) {
            require(lhsType.callee is Type.Constructor)
            val identifier = requireNotNull(lhsType.callee.binder?.identifier)
            val binding = ctx.resolver.resolveTypeVariable(identifier)
            requireNotNull(binding)
            require(binding is TypeBinding.Struct)
            ctx.checker.typeOfStructMembers(binding.declaration)

        } else {
            requireUnreachable()
        }

        val index = members.keys.indexOf(expression.property.name)
        require(index > -1)
        requireNotNull(members[expression.property.name])
        val rhsType = typeOfExpression(expression)
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
        val name: IRName = when (binding) {
            null -> requireUnreachable()
            is ValueBinding.GlobalFunction -> {
                val def = getFunctionDef(binding.declaration)
                def.name
            }
            is ValueBinding.ExternFunction -> {
                val def = lowerExternFunctionDef(binding.declaration)
                def.name
            }
            is ValueBinding.FunctionParam -> {
                val index = binding.index
                assert(index > -1)
                getFunctionDef(binding.declaration).params[index].name
            }
            is ValueBinding.ValBinding -> {
                val ptr = getValBinding(binding.statement)
                val derefName = IRLocalName(ctx.makeUniqueName())
                builder.buildLoad(
                    derefName,
                    ty,
                    builder.buildVariable(ty = Type.RawPtr(ty), location = node.location, name = ptr)
                )
                derefName
            }
            is ValueBinding.Struct -> {
                val structDecl = lowerStructDeclaration(binding.declaration)
                structDecl.globalName
            }
        }
        return builder.buildVariable(ty, node.location, name)

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
            name = IRLocalName(ctx.makeUniqueName())
        )
    }

    private fun typeOfExpression(expression: Expression): Type {
        return ctx.checker.typeOfExpression(expression)
    }

    private fun lowerStatement(statement: Statement) = when (statement) {
        is Statement.Return -> lowerReturnStatement(statement)
        is Statement.Val -> lowerValStatement(statement)
        is Statement.While -> lowerWhileStatement(statement)
        is Statement.If -> lowerIfStatement(statement)
        is Statement.Error -> TODO()
    }

    /**
     * if <condition>
     *      <ifTrue>
     *
     * br <condition> ifTrue: %1, ifFasle: %2
     * %1:
     *  <ifTrue>
     *  jmp %3
     * %2:
     *  <ifFalse>
     *  jmp %3
     * %3: // position at end
     * ...
     *
     */
    private fun lowerIfStatement(statement: Statement.If) {
        val ifTrue = buildBlock()
        val ifFalse = buildBlock()
        val end = buildBlock()

        builder.buildBranch(
            statement.condition.location,
            lowerExpression(statement.condition),
            ifTrue = ifTrue.name,
            ifFalse = ifFalse.name)

        lowerBlock(statement.ifTrue, ifTrue)
        val endLocation = statement.ifFalse?.location ?: statement.ifTrue.location
        builder.buildJump(endLocation, end.name)

        builder.position = ifFalse
        if (statement.ifFalse != null) {
            lowerBlock(statement.ifFalse, ifFalse)
            builder.position = ifFalse
            builder.buildJump(endLocation, end.name)
        } else {
            builder.position = ifFalse
            builder.buildJump(endLocation, end.name)
        }
        builder.position = end
    }

    /**
     * <pre>
     * while (<condition>) <block>
     *
     * br %condition .while_body .while_exit
     * .while_body
     *   ...<block>...
     *   br %condition .while_body .while_exit
     * .while_exit <- position block here after we're done
     *  ...
     *
     */
    private fun lowerWhileStatement(statement: Statement.While) {
        val whileBody = buildBlock()
        val whileExit = buildBlock()

        builder.buildBranch(statement.condition.location, lowerExpression(statement.condition), whileBody.name, whileExit.name)

        lowerBlock(statement.body, whileBody)

        builder.buildBranch(statement.condition.location, lowerExpression(statement.condition), whileBody.name, whileExit.name)

        builder.position = whileExit
    }

    private fun buildBlock(): IRBlock {
        val block = IRBlock(makeBlockName())
        requireNotNull(currentFunction).appendBlock(block)
        return block
    }

    private fun makeLocalName(): IRLocalName {
        return IRLocalName(ctx.makeUniqueName())
    }

    private fun makeBlockName(): IRLocalName {
        return IRLocalName(ctx.makeUniqueName())
    }

    private fun lowerReturnStatement(statement: Statement.Return) {
        builder.buildReturn(lowerExpression(statement.value))
    }

    private val valPointers = mutableMapOf<SourceLocation, IRLocalName>()
    private fun lowerValStatement(statement: Statement.Val) {
        val (name, type) = lowerLocalBinder(statement.binder)
        builder.buildAlloca(type.to, name)
        val ptr = builder.buildVariable(type, statement.location, name)
        val rhs = lowerExpression(statement.rhs)
        builder.buildStore(ptr, rhs)
        valPointers[statement.location] = name
    }

    private fun getValBinding(statement: Statement.Val): IRLocalName {
        return requireNotNull(valPointers[statement.location])
    }

    private fun lowerGlobalBinder(name: Binder): Pair<IRGlobalName, Type> {
        val sourceFile = ctx.getSourceFileOf(name)
        val ty = ctx.checker.typeOfBinder(name)
        return IRGlobalName(sourceFile.moduleName.append(name.identifier.name)) to ty
    }

    private fun lowerLocalBinder(name: Binder): Pair<IRLocalName, Type.RawPtr> {
        val ty = Type.RawPtr(ctx.checker.typeOfBinder(name))
        return IRLocalName(name.identifier.name) to ty
    }

    private fun lowerParamBinder(name: Binder): Pair<IRLocalName, Type> {
        val ty = ctx.checker.typeOfBinder(name)
        return IRLocalName(name.identifier.name) to ty
    }
}

