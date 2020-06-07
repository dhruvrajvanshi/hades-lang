package hadesc.ir

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.checker.ImplementationBinding
import hadesc.checker.PropertyBinding
import hadesc.context.Context
import hadesc.exhaustive
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.profile
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class IRGen(private val ctx: Context) {
    private val module = IRModule()
    private val definitions = mutableListOf<IRDefinition>()
    private val loweredSourceFileSet = mutableSetOf<SourcePath>()
    private val builder = IRBuilder()
    private var currentFunction: IRFunctionDef? = null

    fun generate(): IRModule = profile("IRGen::generate") {
        ctx.forEachSourceFile { lowerSourceFile(it) }
        return module
    }

    private fun lowerSourceFile(sourceFile: SourceFile) = profile("IRGen::lowerSourceFile(${sourceFile.location.file})") {
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
        is Declaration.ConstDefinition -> {
            lowerConstDeclaration(declaration)
            Unit
        }
        is Declaration.Interface -> lowerInterfaceDecl(declaration)
        is Declaration.Implementation -> {
            lowerImplementation(declaration)
        }
        is Declaration.Enum -> lowerEnumDecl(declaration)
    }

    private val isEnumDeclLowered = mutableSetOf<SourceLocation>()
    private val enumDataTypes = mutableMapOf<SourceLocation, Type>()
    private fun lowerEnumDecl(declaration: Declaration.Enum) {
        if (declaration.location in isEnumDeclLowered) {
            return
        }
        val instanceType = ctx.checker.typeOfEnumInstance(declaration)
        val enumName = ctx.resolver.qualifiedEnumName(declaration)
        val typeParams = declaration.typeParams?.map { lowerTypeParam(it) }
        val caseTypes = mutableListOf<Type>()
        declaration.cases.forEachIndexed { index, case ->
            val dataName = IRGlobalName(enumName.append(case.name.identifier.name).append(ctx.makeName("Data")))
            val constructorName = IRGlobalName(enumName.append(case.name.identifier.name))
            val caseInstanceType = if (typeParams == null) {
                Type.Constructor(
                        case.name,
                        dataName.name,
                        params = null
                )
            } else {
                Type.Application(
                        Type.Constructor(case.name, dataName.name, params = typeParams.map { Type.Param(it.binder) }),
                        typeParams.map { Type.ParamRef(it.binder) }
                )
            }
            if (case.params.isEmpty()) {
                module.addConstDef(dataName, caseInstanceType, IRCIntConstant(Type.CInt, case.name.location, 0))
            } else {
                val constructorType = Type.Function(
                        typeParams = typeParams?.map { Type.Param(it.binder) },
                        from = case.params.map { ctx.checker.annotationToType(it) },
                        to = caseInstanceType,
                        receiver = null
                )
                val fields = case.params.mapIndexed { paramIndex, annotation ->
                    ctx.makeName(paramIndex.toString()) to ctx.checker.annotationToType(annotation)
                }.toMap()
                module.addStructDef(
                        constructorType,
                        caseInstanceType,
                        dataName,
                        typeParams,
                        fields
                )

                val constructorFn = module.addGlobalFunctionDef(
                    location = case.name.location,
                    name = constructorName,
                    type = constructorType.copy(to = instanceType),
                    typeParams = typeParams,
                    constraints = listOf(),
                    receiverType = null,
                    entryBlock = IRBlock(IRLocalName(ctx.makeName("entry"))),
                    params = case.params.mapIndexed { paramIndex, caseParam ->
                        IRParam(
                                type = ctx.checker.annotationToType(caseParam),
                                location = caseParam.location,
                                functionName = constructorName,
                                index = paramIndex,
                                name = IRLocalName(ctx.makeName(paramIndex.toString()))
                        )
                    }
                )
            }
            caseTypes.add(caseInstanceType)
        }
        val constructorType = Type.Function(
                receiver = null,
                typeParams = typeParams?.map { Type.Param(it.binder) },
                to = instanceType,
                from = listOf(Type.CInt, Type.UntaggedUnion(caseTypes))
        )
        val dataType = Type.UntaggedUnion(caseTypes)
        val fields = mapOf(
                ctx.makeName("tag") to Type.CInt,
                ctx.makeName("data") to dataType
        )
        enumDataTypes[declaration.location] = dataType
        module.addStructDef(
                constructorType,
                instanceType,
                IRGlobalName(enumName),
                typeParams,
                fields
        )
    }

    private fun lowerInterfaceDecl(declaration: Declaration.Interface) {
        val name = globalBinderName(declaration.name)
        val thisTypeParamBinder = Binder(declaration.name.identifier.copy(name = ctx.makeName("This")))
        val typeParams = buildList {
            add(IRTypeParam(IRLocalName(ctx.makeName("This")), thisTypeParamBinder))
            declaration.typeParams?.forEach {
                add(lowerTypeParam(it))
            }
        }
        val interfaceRef = IRInterfaceRef(
                name,
                typeArgs = declaration.typeParams?.map { Type.ParamRef(it.binder) } ?: listOf()
        )
        val thisType = Type.ParamRef(thisTypeParamBinder)
        val instanceType = typeOfInterfaceInstance(interfaceRef, thisType)
        val fields = declaration.members.map {
            exhaustive(when (it) {
                is Declaration.Interface.Member.FunctionSignature -> {
                    val fnType = ctx.checker.typeOfFunctionSignature(it.signature).applySubstitution(mapOf(), thisType)
                    require(fnType is Type.Function)
                    it.signature.name.identifier.name to
                            Type.RawPtr(fnType)
                }
            })
        }.toMap()
        val constructorType = Type.Function(
                typeParams = listOf(
                        Type.Param(thisTypeParamBinder)
                ) + (declaration.typeParams?.map { Type.Param(it.binder) } ?: listOf()),
                constraints = emptyList(),
                to = instanceType,
                receiver = null,
                from = fields.values.toList()
        )
        module.addStructDef(
                constructorType = constructorType,
                instanceType = instanceType,
                fields = fields,
                typeParams = typeParams,
                name = name
        )
    }

    private fun getImplTypeAndName(declaration: Declaration.Implementation): Pair<IRGlobalName, Type> {
        val name = implName(declaration)
        val type = implType(declaration)
        return name to type
    }

    private fun implType(declaration: Declaration.Implementation): Type {
        return getInterfaceRefType(declaration.interfaceRef, ctx.checker.annotationToType(declaration.forType))
    }

    private fun resolveInterfaceDecl(interfaceRef: InterfaceRef): Declaration.Interface {
        val interfaceDecl = ctx.resolver.resolveDeclaration(interfaceRef.path)
        require(interfaceDecl is Declaration.Interface)
        return interfaceDecl
    }

    private fun getInterfaceRefType(interfaceRef: InterfaceRef, thisType: Type): Type {
        return typeOfInterfaceInstance(lowerInterfaceRef(interfaceRef), thisType)
    }

    private fun lowerInterfaceRef(interfaceRef: InterfaceRef): IRInterfaceRef {
        val decl = requireNotNull(ctx.resolver.resolveDeclaration(interfaceRef.path))
        require(decl is Declaration.Interface)
        val name = ctx.resolver.qualifiedInterfaceName(decl)
        return IRInterfaceRef(
                IRGlobalName(name),
                interfaceRef.typeArgs?.map { ctx.checker.annotationToType(it) } ?: listOf()
        )
    }

    private fun typeOfInterfaceInstance(interfaceRef: IRInterfaceRef, thisType: Type): Type {
        val interfaceDecl = ctx.checker.getInterfaceDecl(interfaceRef.name.name)
        val interfaceName = interfaceRef.name.name
        require(interfaceDecl.typeParams?.size ?: 0 == interfaceRef.typeArgs.size)
        val interfaceTypeParams = interfaceDecl.typeParams?.map {
            require(it.bound == null)
            Type.Param(it.binder)
        } ?: listOf()
        return Type.Application(
                callee = Type.Constructor(
                        binder = interfaceDecl.name,
                        name = interfaceName,
                        params = listOf(Type.Param(Binder(interfaceDecl.name.identifier.copy(name = ctx.makeName("This"))))) + interfaceTypeParams
                ),
                args = listOf(thisType) + interfaceRef.typeArgs
        )
    }

    private val implNames = mutableMapOf<SourceLocation, IRGlobalName>()
    private fun implName(declaration: Declaration.Implementation): IRGlobalName = implNames.getOrPut(declaration.location) {
        // TODO: Pick a properly mangled name here
        return IRGlobalName(QualifiedName(listOf(ctx.makeName("impl$" + implType(declaration).prettyPrint()))))
    }

    private fun lowerImplementation(declaration: Declaration.Implementation) {
        val type = implType(declaration)
        val name = implName(declaration)
        val interfaceDecl = resolveInterfaceDecl(declaration.interfaceRef)
        val values = buildList {
            for (member in interfaceDecl.members) {
                require(member is Declaration.Interface.Member.FunctionSignature)
                val implFuncDef = declaration.members.find {
                    when(it) {
                        is Declaration.Implementation.Member.FunctionDef ->
                            it.functionDef.name.identifier.name == member.signature.name.identifier.name
                    }
                }
                require(implFuncDef is Declaration.Implementation.Member.FunctionDef)
                val functionDef = lowerGlobalFunctionDef(implFuncDef.functionDef, prefix = name.name)
                add(builder.buildVariable(
                        ty = Type.RawPtr(functionDef.type),
                        location = implFuncDef.functionDef.location,
                        name = functionDef.name

                ))
            }
        }
        val initializer = IRAggregate(
                type = type,
                location = declaration.location,
                values = values
        )
        module.addConstDef(
                name,
                type,
                initializer = initializer
        )
    }

    private val loweredConstDefs = mutableMapOf<SourceLocation, IRConstDef>()
    private fun lowerConstDeclaration(declaration: Declaration.ConstDefinition): IRConstDef =
        loweredConstDefs.computeIfAbsent(declaration.location) {
            val initializer = lowerExpression(declaration.initializer)
            val (name, type) = lowerGlobalBinder(declaration.name)
            module.addConstDef(name, type, initializer)
        }

    private val declaredExternDefs = mutableMapOf<SourceLocation, IRExternFunctionDef>()
    private fun lowerExternFunctionDef(declaration: Declaration.ExternFunctionDef): IRExternFunctionDef =
        declaredExternDefs.computeIfAbsent(declaration.location) {
            val (name, type) = lowerGlobalBinder(declaration.binder)
            require(type is Type.Function)
            val def = module.addExternFunctionDef(
                name,
                type,
                externName = declaration.externName.name
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
                is Declaration.Struct.Member.Field -> it.binder.identifier.name to ctx.checker.annotationToType(it.typeAnnotation)
            }
        }.toMap()
        val (name, type) = lowerGlobalBinder(declaration.binder)
        val instanceType = ctx.checker.typeOfStructInstance(declaration)
        val typeParams = declaration.typeParams?.map { lowerTypeParam(it) }
        val def = module.addStructDef(
            type as Type.Function,
            instanceType,
            name,
            typeParams = typeParams,
            fields = fields
        )
        definitions.add(def)
        addedStructDefs[declaration.location] = def
        lowerStructInitializer(
                declaration,
                type,
                instanceType,
                name,
                typeParams)
        return def
    }

    private fun lowerStructInitializer(
            declaration: Declaration.Struct,
            constructorType: Type.Function,
            instanceType: Type,
            name: IRGlobalName,
            typeParams: List<IRTypeParam>?
    ) {
        val initializerType = structInitializerType(constructorType, instanceType)
        val initializerName = structInitializerName(name)
        val fn = module.addGlobalFunctionDef(
                declaration.location,
                name = initializerName,
                typeParams = typeParams,
                type = initializerType,
                receiverType = null,
                params = initializerType.from.mapIndexed { index, type ->
                    IRParam(makeLocalName(), type, declaration.location, initializerName, index)
                },
                entryBlock = IRBlock(),
                constraints = listOf()
        )
        val oldPosition = builder.position
        builder.withinBlock(fn.entryBlock) {
            val thisParam = fn.params[0]
            val thisPtr = builder.buildVariable(
                    ty = thisParam.type,
                    location = thisParam.location,
                    name = thisParam.name
            )
            fn.params.drop(1).forEachIndexed { index, param ->
                builder.buildStore(
                        ptr = IRGetElementPointer(param.type, param.location, ptr = thisPtr, offset = index),
                        value = builder.buildVariable(param.type, param.location, param.name)
                )
            }
            builder.buildRetVoid()
        }
        builder.position = oldPosition
    }

    private fun structInitializerName(name: IRGlobalName): IRGlobalName {
        return IRGlobalName(name.name.append(ctx.makeName("init")))
    }

    private fun structInitializerType(constructorType: Type.Function, instanceType: Type): Type.Function {
        return constructorType.copy(
                from = listOf(Type.RawPtr(instanceType)) + constructorType.from,
                to = Type.Void
        )
    }

    private val declaredFunctionDefs = mutableMapOf<SourceLocation, IRFunctionDef>()

    private fun getFunctionDef(def: Declaration.FunctionDef, prefix: QualifiedName? = null): IRFunctionDef {
        return declaredFunctionDefs.computeIfAbsent(def.location) {
            val (unprefixedName, type) = lowerGlobalBinder(def.name)
            val functionName = if (prefix == null) unprefixedName
            else IRGlobalName(unprefixedName.name.withPrefix(prefix))
            require(type is Type.Function)
            val params = def.params.mapIndexed { index, param -> lowerParam(param, functionName, index) }
            val receiverType = def.signature.thisParam?.annotation?.let { ctx.checker.annotationToType(it) }

            val function = module.addGlobalFunctionDef(
                def.signature.location,
                functionName,
                type,
                receiverType = receiverType,
                typeParams = def.typeParams?.map { lowerTypeParam(it) },
                params = params,
                entryBlock = IRBlock(),
                constraints = type.constraints.map {
                    IRConstraint(
                            makeLocalName(),
                            IRTypeParam(
                                    IRLocalName(it.param.binder.identifier.name),
                                    it.param.binder
                            ),
                            IRInterfaceRef(
                                name = IRGlobalName(it.interfaceName),
                                typeArgs = it.args
                            ),
                            type = typeOfInterfaceInstance(interfaceRefOf(it), Type.ParamRef(it.param.binder)),
                            location = def.signature.location
                    )
                }
            )
            definitions.add(function)
            function
        }

    }

    private fun interfaceRefOf(constraint: Type.Constraint): IRInterfaceRef {
        return IRInterfaceRef(IRGlobalName(constraint.interfaceName), typeArgs = constraint.args)
    }

    private fun lowerTypeParam(typeParam: TypeParam): IRTypeParam {
        return IRTypeParam(IRLocalName(typeParam.binder.identifier.name), typeParam.binder)
    }

    private fun lowerGlobalFunctionDef(def: Declaration.FunctionDef, prefix: QualifiedName? = null): IRFunctionDef {
        val function = getFunctionDef(def, prefix)
        currentFunction = function
        function.entryBlock = lowerBlock(def.body, function.entryBlock)
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
            is Expression.This -> lowerThisExpression(expression)
            is Expression.NullPtr -> IRNullPtr(typeOfExpression(expression), expression.location)
            is Expression.IntLiteral -> IRCIntConstant(
                typeOfExpression(expression),
                expression.location,
                expression.value
            )
            is Expression.Not -> {
                val ty = typeOfExpression(expression)
                val name = makeLocalName()
                builder.buildNot(ty, expression.location, name, lowerExpression(expression.expression))
                builder.buildVariable(ty, expression.location, name)
            }
            is Expression.BinaryOperation -> {
                if (isShortCircuitingOperator(expression.operator)) {
                    lowerShortCircuitingOperator(expression)
                } else {
                    val ty = typeOfExpression(expression)
                    val lhs = lowerExpression(expression.lhs)
                    val rhs = lowerExpression(expression.rhs)
                    val name = makeLocalName()
                    builder.buildBinOp(ty, name, lhs, expression.operator, rhs)
                    builder.buildVariable(ty, expression.location, name)
                }
            }
            is Expression.SizeOf -> IRSizeOf(
                    type = Type.Size,
                    location = expression.location,
                    ofType = ctx.checker.annotationToType(expression.type))
            is Expression.AddressOf -> lowerAddressOf(expression)
            is Expression.Load -> lowerLoad(expression)
            is Expression.PointerCast -> lowerPointerCast(expression)
            is Expression.If -> lowerIfExpression(expression)
            is Expression.TypeApplication -> lowerTypeApplication(expression)
            is Expression.Match -> lowerMatchExpression(expression)
            is Expression.New -> lowerNewExpression(expression)
        }
        return lowered
    }

    private fun lowerNewExpression(expression: Expression.New): IRValue {
        val constructorDecl = ctx.resolver.resolveDeclaration(expression.qualifiedPath)
        require(constructorDecl is Declaration.Struct)
        val structDef = lowerStructDeclaration(constructorDecl)

        val type = typeOfExpression(expression)
        require(type is Type.RawPtr)
        val thisPtrName = makeLocalName()
        val allocUninitializedName = IRGlobalName(QualifiedName(listOf(
                ctx.makeName("memory"),
                ctx.makeName("unsafe_allocate_unitialized")
        )))
        val allocatorBinding = module.resolveGlobal(allocUninitializedName)
        require(allocatorBinding is IRBinding.FunctionDef)

        builder.buildCall(
                type = type,
                location = expression.location,
                name = thisPtrName,
                typeArgs = listOf(type.to),
                args = listOf(),
                callee = builder.buildVariable(allocatorBinding.type, expression.location, allocUninitializedName)
        )
        val thisPtr = builder.buildVariable(type, expression.location, thisPtrName)

        val args = listOf(thisPtr) + expression.args.map { lowerExpression(it.expression) }
        val typeArgs = ctx.checker.getTypeArgs(expression)

        val calleeDef = module.resolveGlobal(structInitializerName(structDef.globalName))
        require(calleeDef is IRBinding.FunctionDef)
        val calleeName = calleeDef.def.name

        require(structDef.constructorType is Type.Function)
        val initializerType = calleeDef.def.type
        val initializer = builder.buildVariable(
                initializerType,
                expression.qualifiedPath.location,
                calleeName
        )

        builder.buildCall(
                Type.Void,
                location = expression.location,
                callee = initializer,
                args = args,
                typeArgs = typeArgs,
                name = makeLocalName()
        )

        return thisPtr
    }

    private fun lowerMatchExpression(expression: Expression.Match): IRValue {
        val value = lowerExpression(expression.value)
        val valueType = value.type
        val (constructorType, typeArgs) = if (valueType is Type.Application) {
            valueType.callee to valueType.args
        } else if (valueType is Type.Constructor) {
            valueType to listOf()
        } else {
            requireUnreachable()
        }
        val enumDeclaration = ctx.resolver.resolveDeclaration(constructorType.name)
        require(enumDeclaration is Declaration.Enum)

        val resultPtrName = makeLocalName()

        builder.buildAlloca(valueType, resultPtrName)
        val resultPtr = IRVariable(type = Type.RawPtr(valueType), name = resultPtrName, location = expression.value.location)


        val tag = builder.buildGetStructField(Type.CInt, expression.value.location, value, ctx.makeName("tag"), 0)
        val data = builder.buildGetStructField(
                enumDataType(enumDeclaration),
                expression.value.location,
                value,
                ctx.makeName("data"),
                1
        )

        val branches = enumDeclaration.cases.mapIndexed { index, case ->
            index to buildBlock()
        }.toMap()


        builder.buildSwitch(
                expression.location,
                tag,
                branches.entries.sortedBy { it.key }.map { it.value.name }
        )

        for (arm in expression.arms) {
            exhaustive(when (arm.pattern) {
                is Pattern.DotName -> {
                    val tagValue = enumDeclaration.cases.indexOfFirst { it.name.identifier.name == arm.pattern.identifier.name }
                    val case = enumDeclaration.cases.find { it.name.identifier.name == arm.pattern.identifier.name }
                    require(tagValue > -1)
                    require(case != null)
                    val block = requireNotNull(branches[tagValue])

                    builder.withinBlock(block) {
                        arm.pattern.params.forEachIndexed { fieldIndex, pattern ->
                            require(pattern is Pattern.Name)
                            val name = IRLocalName(pattern.binder.identifier.name)
                            val fieldType = TODO()
                            val ptr = builder.buildVariable(Type.RawPtr(fieldType), pattern.location, name)
                            builder.buildAlloca(fieldType, name)
                            patternVars[pattern.location] = name
                            val value = builder.buildGetStructField(
                                    fieldType,
                                    pattern.location,
                                    data,
                                    field = null,
                                    index = fieldIndex
                            )
                            builder.buildStore(ptr, value)
                        }
                        val armValue = lowerExpression(arm.expression)
                        builder.buildStore(resultPtr, armValue)
                    }
                }
                is Pattern.Name -> TODO()
                is Pattern.Else -> TODO()
                else -> requireUnreachable()
            })
        }
        TODO()
    }

    private fun enumDataType(enumDeclaration: Declaration.Enum): Type {
        lowerEnumDecl(enumDeclaration)
        return requireNotNull(enumDataTypes[enumDeclaration.location])
    }

    private fun lowerTypeApplication(expression: Expression.TypeApplication): IRValue {
        return lowerExpression(expression.lhs)
    }

    private fun lowerPointerCast(expression: Expression.PointerCast): IRValue {
        val toPointerOfType = ctx.checker.annotationToType(expression.toType)

        return IRPointerCast(
            type = Type.RawPtr(toPointerOfType),
            location = expression.location,
            toPointerOfType = toPointerOfType,
            arg = lowerExpression(expression.arg)
        )

    }

    private fun lowerLoad(expression: Expression.Load): IRValue {
        val ptr = lowerExpression(expression.expression)
        val name = makeLocalName()
        val ty = typeOfExpression(expression.expression)
        require(ty is Type.RawPtr)
        builder.buildLoad(name, ty.to, ptr)
        return IRVariable(
                type = ty.to,
                location = expression.location,
                name = name
        )
    }

    private fun lowerAddressOf(expression: Expression.AddressOf): IRValue {
        require(expression.expression is Expression.Var)
        return resolveLocalVariablePointer(expression.expression.name)
    }

    private fun resolveLocalVariablePointer(name: Identifier): IRValue {
        val binding = ctx.resolver.resolve(name)
        require(binding is Binding.ValBinding)
        val ptrName = requireNotNull(valPointers[binding.statement.location])
        return IRVariable(
                type = lowerLocalBinder(binding.statement.binder).second,
                location = name.location,
                name = ptrName
        )
    }

    private fun isShortCircuitingOperator(operator: BinaryOperator): Boolean {
        return operator == BinaryOperator.AND || operator == BinaryOperator.OR
    }

    /**
     * %condition = alloca Bool
     * store %condition lhs
     * %lhs = load %condition

     * .done:
     * load %condition
     */
    private fun lowerShortCircuitingOperator(expression: Expression.BinaryOperation): IRValue {
        val conditionName = makeLocalName()
        val conditionPtr = IRVariable(Type.RawPtr(Type.Bool), expression.lhs.location, conditionName)
        val lhsName = makeLocalName()
        val lhs = IRVariable(Type.Bool, expression.location, lhsName)
        val done = buildBlock()
        // %condition = alloca Bool
        // store %condition lhs
        // %lhs = load %condition
        builder.buildAlloca(Type.Bool, conditionName)
        builder.buildStore(ptr = conditionPtr, value = lowerExpression(expression.lhs))
        builder.buildLoad(name = lhsName, ptr = conditionPtr, type = Type.Bool)

        when (expression.operator) {
            BinaryOperator.AND -> {
                // br %lhs if_true:.and_rhs if_false:.and_short_circuit
                // .and_rhs:
                //   store %condition rhs
                //   jmp .done
                // .and_short_circuit:
                //   jmp .done
                val andRHS = buildBlock()
                val andShortCircuit = buildBlock()
                builder.buildBranch(expression.location, lhs, ifTrue = andRHS.name, ifFalse = andShortCircuit.name)
                builder.withinBlock(andRHS) {
                    builder.buildStore(ptr = conditionPtr, value = lowerExpression(expression.rhs))
                    builder.buildJump(expression.lhs.location, done.name)
                }
                builder.withinBlock(andShortCircuit) {
                    builder.buildJump(expression.rhs.location, done.name)
                }

            }
            BinaryOperator.OR -> {
                // br %lhs if_true:.or_short_circuit if_false:.or_rhs
                // .or_short_circuit:
                //   jmp .done
                // .or_rhs:
                //   store %condition rhs
                //   jmp .done
                val orShortCircuit = buildBlock()
                val orRHS = buildBlock()

                builder.buildBranch(expression.location, lhs, ifTrue = orShortCircuit.name, ifFalse = orRHS.name)
                builder.withinBlock(orShortCircuit) {
                    builder.buildJump(expression.lhs.location, done.name)
                }
                builder.withinBlock(orRHS) {
                    builder.buildStore(ptr = conditionPtr, value = lowerExpression(expression.rhs))
                    builder.buildJump(expression.rhs.location, done.name)
                }
            }
            else -> {
                requireUnreachable()
            }
        }


        builder.position = done
        val resultName = makeLocalName()
        builder.buildLoad(resultName, Type.Bool, ptr = conditionPtr)

        return IRVariable(Type.Bool, expression.location, resultName)

    }

    private fun lowerThisExpression(expression: Expression.This): IRValue {
        return builder.buildVariable(
            name = IRLocalName(ctx.makeName("this")),
            ty = typeOfExpression(expression),
            location = expression.location
        )
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

    private fun lowerProperty(expression: Expression.Property): IRValue {
        return when (val binding = requireNotNull(ctx.checker.getPropertyBinding(expression))) {
            is PropertyBinding.Global -> lowerBindingRef(typeOfExpression(expression), expression, binding.binding)
            is PropertyBinding.StructField -> {
                lowerStructFieldBinding(expression, binding)
            }
            is PropertyBinding.GlobalExtensionFunction -> {
                lowerGlobalExtensionFunctionBinding(expression, binding)
            }
            is PropertyBinding.InterfaceExtensionFunction -> {
                lowerInterfaceExtensionFunction(expression, binding)
            }
        }
    }

    private fun lowerInterfaceExtensionFunction(expression: Expression.Property, binding: PropertyBinding.InterfaceExtensionFunction): IRValue {
        val implRef = lowerImplBinding(expression.location, binding.implementationBinding)
        require(binding.type is Type.Function)
        requireNotNull(binding.type.receiver)
        val memberType = typeOfImplMember(binding.implementationBinding.interfaceRef, binding.memberIndex, binding.type.receiver)

        val memberRef = builder.buildGetStructField(
                ty = memberType,
                location = expression.location,
                lhs = implRef,
                index = binding.memberIndex,
                field = null
        )
        return builder.buildMethodRef(
                type = binding.type,
                method = memberRef,
                thisArg = lowerExpression(expression.lhs),
                location = expression.location
        )
    }

    private fun typeOfImplMember(interfaceRef: InterfaceRef, memberIndex: Int, thisType: Type): Type {
        val interfaceDecl = resolveInterfaceDecl(interfaceRef)

        val substitution = interfaceDecl.typeParams?.zip(interfaceRef.typeArgs ?: listOf())?.map {
            it.first.binder.location to ctx.checker.annotationToType(it.second)
        }?.toMap() ?: mapOf()
        return when (val member = interfaceDecl.members[memberIndex]) {
            is Declaration.Interface.Member.FunctionSignature -> {
                val fnType = ctx.checker.typeOfFunctionSignature(member.signature).applySubstitution(substitution, thisType)
                require(fnType is Type.Function)
                Type.RawPtr(fnType)
            }
        }
    }

    private fun lowerImplBinding(location: SourceLocation, implementationBinding: ImplementationBinding): IRValue {
        return when(implementationBinding) {
            is ImplementationBinding.TypeBound -> {
                val functionDef = getFunctionDef(implementationBinding.functionDef)
                val typeParams = requireNotNull(functionDef.typeParams)
                val typeParam = typeParams[implementationBinding.typeParamIndex]
                val constraint = requireNotNull(functionDef.signature.constraints.find { it.typeParam == typeParam })
                builder.buildVariable(
                        ty = typeOfConstraint(constraint),
                        location = location,
                        name = constraint.name
                )
            }
            is ImplementationBinding.GlobalImpl -> {
                getImplAsValue(location, implementationBinding.implDef)
            }
        }
    }

    private fun typeOfConstraint(constraint: IRConstraint): Type {
        return typeOfInterfaceInstance(
                constraint.interfaceRef,
                Type.ParamRef(constraint.typeParam.binder))
    }

    private fun getImplAsValue(location: SourceLocation, implDef: Declaration.Implementation): IRValue {
        val (name, type) = getImplTypeAndName(implDef)
        return builder.buildVariable(
                ty = type,
                name = name,
                location = location
        )
    }

    private fun lowerGlobalExtensionFunctionBinding(expression: Expression.Property, binding: PropertyBinding.GlobalExtensionFunction): IRValue {
        val lhs = lowerExpression(expression.lhs)
        val def = binding.def
        val (fnName, methodTy) = lowerGlobalBinder(def.name)
        require(methodTy is Type.Function)
        require(methodTy.receiver != null)
        val typeArgs = ctx.checker.getTypeArgs(expression)
        if (def.typeParams != null) {
            require(typeArgs != null)
        }
        return builder.buildMethodRef(
                type = methodTy,
                location = expression.location,
                thisArg = lhs,
                method = IRVariable(name = fnName, type = methodTy, location = expression.lhs.location)
        )
    }

    private fun lowerStructFieldBinding(expression: Expression.Property, binding: PropertyBinding.StructField): IRValue {
        val rhsType = typeOfExpression(expression)
        val lhs = lowerExpression(expression.lhs)
        val index = binding.structDecl.members.indexOfFirst { it === binding.member }
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

    private val patternVars = mutableMapOf<SourceLocation, IRLocalName>()
    private fun lowerBindingRef(ty: Type, node: HasLocation, binding: Binding?): IRValue {
        val name: IRName = when (binding) {
            null -> requireUnreachable()
            is Binding.GlobalFunction -> {
                val def = getFunctionDef(binding.declaration)
                def.name
            }
            is Binding.ExternFunction -> {
                val def = lowerExternFunctionDef(binding.declaration)
                def.name
            }
            is Binding.FunctionParam -> {
                val index = binding.index
                assert(index > -1)
                val indexWithThis = if (binding.declaration.thisParam != null) index + 1 else index
                getFunctionDef(binding.declaration).params[indexWithThis].name
            }
            is Binding.ValBinding -> {
                val ptr = getValBinding(binding.statement)
                val derefName = makeLocalName()
                builder.buildLoad(
                    derefName,
                    ty,
                    builder.buildVariable(ty = Type.RawPtr(ty), location = node.location, name = ptr)
                )
                derefName
            }
            is Binding.Struct -> {
                val structDecl = lowerStructDeclaration(binding.declaration)
                structDecl.globalName
            }
            is Binding.GlobalConst -> {
                lowerConstDeclaration(binding.declaration).name
            }
            is Binding.EnumCaseConstructor -> {
                lowerEnumCaseConstructorBinding(binding)
            }
            is Binding.Pattern -> {
                requireNotNull(patternVars[binding.pattern.location])
            }
        }
        return builder.buildVariable(ty, node.location, name)

    }

    private fun lowerEnumCaseConstructorBinding(binding: Binding.EnumCaseConstructor): IRName {
        val enumName = globalBinderName(binding.declaration.name)
        val caseName = binding.case.name.identifier.name
        return IRGlobalName(enumName.name.append(caseName))
    }

    private fun lowerCall(expression: Expression.Call): IRValue {
        val callee = lowerExpression(expression.callee)
        val args = expression.args.map { lowerExpression(it.expression) }
        val type = typeOfExpression(expression)
        val constraintBindings = ctx.checker.getConstraintBindings(expression)
        return builder.buildCall(
            type,
            expression.location,
            callee = callee,
            typeArgs = ctx.checker.getTypeArgs(expression),
            args = args + constraintBindings.map { lowerImplBinding(callee.location, it) },
            name = makeLocalName()
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
        is Statement.LocalAssignment -> lowerLocalAssignment(statement)
        is Statement.Error -> requireUnreachable()
        is Statement.MemberAssignment -> lowerMemberAssignment(statement)
    }

    private fun lowerMemberAssignment(statement: Statement.MemberAssignment) {
        require(statement.lhs.lhs is Expression.Var)
        val valuePtr = resolveLocalVariablePointer(statement.lhs.lhs.name)
        val lhsType = typeOfExpression(statement.lhs)
        val propertyBinding = ctx.checker.getPropertyBinding(statement.lhs)
        require(propertyBinding is PropertyBinding.StructField)
        val offset = propertyBinding.memberIndex
        val memberPtr = IRGetElementPointer(
            Type.RawPtr(lhsType),
            location = statement.lhs.location,
            offset = offset,
            ptr = valuePtr
        )
        builder.buildStore(ptr = memberPtr, value = lowerExpression(statement.value))
    }

    private fun lowerLocalAssignment(statement: Statement.LocalAssignment) {
        builder.buildStore(ptr = resolveLocalVariablePointer(statement.name), value = lowerExpression(statement.value))
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
            ifFalse = ifFalse.name
        )

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

    private fun lowerIfExpression(expr: Expression.If): IRValue {
        val resultPtrName = makeLocalName()
        val type = ctx.checker.typeOfExpression(expr.trueBranch)
        builder.buildAlloca(type, resultPtrName)
        val condition = lowerExpression(expr.condition)
        val ifTrue = buildBlock()
        val ifFalse = buildBlock()
        val resultBlock = buildBlock()
        val resultPtr = builder.buildVariable(type, expr.location, resultPtrName)

        builder.buildBranch(expr.location, condition, ifTrue.name, ifFalse.name);

        builder.withinBlock(ifTrue) {
            builder.buildStore(ptr = resultPtr, value = lowerExpression(expr.trueBranch))
            builder.buildJump(expr.trueBranch.location, resultBlock.name)
        }

        builder.withinBlock(ifFalse) {
            builder.buildStore(ptr = resultPtr, value = lowerExpression(expr.falseBranch))
            builder.buildJump(expr.falseBranch.location, resultBlock.name)
        }

        val resultName = makeLocalName()
        builder.withinBlock(resultBlock) {
            builder.buildLoad(resultName, type, resultPtr);
        }
        return builder.buildVariable(
                type,
                expr.location,
                resultName
        )
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

        builder.buildBranch(
            statement.condition.location,
            lowerExpression(statement.condition),
            whileBody.name,
            whileExit.name
        )

        lowerBlock(statement.body, whileBody)

        builder.buildBranch(
            statement.condition.location,
            lowerExpression(statement.condition),
            whileBody.name,
            whileExit.name
        )

        builder.position = whileExit
    }

    private fun buildBlock(): IRBlock {
        val block = IRBlock(_makeBlockName())
        requireNotNull(currentFunction).appendBlock(block)
        return block
    }

    private fun makeLocalName(): IRLocalName {
        return IRLocalName(ctx.makeUniqueName())
    }

    private fun _makeBlockName(): IRLocalName {
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
        val binderType = ctx.checker.typeOfBinder(name)
        return globalBinderName(name) to binderType
    }

    private fun globalBinderName(name: Binder): IRGlobalName {
        val sourceFile = ctx.getSourceFileOf(name)
        return IRGlobalName(sourceFile.moduleName.append(name.identifier.name))
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

