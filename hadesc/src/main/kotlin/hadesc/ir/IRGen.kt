package hadesc.ir

import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.ValueBinding
import hadesc.types.Type

class IRGen(val ctx: Context) {
    private val module = IRModule()
    private val loweredSourceFileSet = mutableSetOf<SourcePath>()
    private val builder = IRBuilder(module)

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

    private fun lowerDeclaration(declaration: Declaration): Unit = when (declaration) {
        is Declaration.Error -> requireUnreachable()
        is Declaration.ImportAs -> {
        }
        is Declaration.FunctionDef -> lowerGlobalFunctionDef(declaration)
        is Declaration.ExternFunctionDef -> lowerExternFunctionDef(declaration)
        is Declaration.Struct -> TODO()
    }

    private fun lowerExternFunctionDef(declaration: Declaration.ExternFunctionDef) {
        val ty = ctx.checker.typeOfBinder(declaration.binder)
        module.addExternFunction(
            declaration.location,
            nameOfGlobalIdent(ty, declaration.externName), ty
        )
    }

    private fun lowerGlobalFunctionDef(def: Declaration.FunctionDef) {
        val loweredName = nameOfGlobalBinder(def.name)
        val ty = ctx.checker.typeOfBinder(def.name)
        val function = module.addFunction(def.location, loweredName, ty)
        val block = function.addBasicBlock("entry")
        builder.positionAtStart(block)
        lowerBlock(block, def.body)
    }

    private fun lowerBlock(block: IRBasicBlock, body: Block) {
        for (member in body.members) {
            lowerBlockMember(member)
        }
    }

    private fun lowerBlockMember(member: Block.Member) = when (member) {
        is Block.Member.Expression -> {
            lowerExpression(member.expression)
            Unit
        }
        is Block.Member.Statement -> lowerStatement(member.statement)
    }

    private fun lowerExpression(expression: Expression): IRExpression = when (expression) {
        is Expression.Error -> requireUnreachable()
        is Expression.Var -> lowerVar(expression)
        is Expression.Call -> lowerCall(expression)
        is Expression.Property -> lowerProperty(expression)
        is Expression.ByteString -> lowerByteString(expression)
        is Expression.BoolLiteral -> TODO()
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
                    val propertyBinding = ctx.resolver.findInSourceFile(
                        expression.property, sourceFile
                    )
                    val qualifiedName = requireNotNull(propertyBinding)
                    nameOfBinding(qualifiedName)
                }
                else -> lowerRuntimePropertyAccess(expression)
            }
        }
        else -> {
            lowerRuntimePropertyAccess(expression)
        }
    }

    private fun nameOfBinding(binding: ValueBinding): IRValueName = when (binding) {
        is ValueBinding.GlobalFunction -> IRGlobalName(
            ctx.checker.typeOfBinding(binding), binding.qualifiedName
        )
        is ValueBinding.ExternFunction -> IRGlobalName(
            ctx.checker.typeOfBinding(binding),
            binding.qualifiedName
        )
        is ValueBinding.FunctionParam -> TODO()
        is ValueBinding.ImportAs -> TODO()
        is ValueBinding.ValBinding -> TODO()
        is ValueBinding.Struct -> IRGlobalName(
            ctx.checker.typeOfBinding(binding),
            binding.qualifiedName
        )
    }

    private fun lowerRuntimePropertyAccess(expression: Expression.Property): IRExpression {
        val lhs = lowerExpression(expression.lhs)
        TODO()
    }

    private fun lowerVar(variable: Expression.Var): IRExpression =
        when (val binding = ctx.resolver.getBinding(variable.name)) {
            is ValueBinding.GlobalFunction -> {
                TODO()
            }
            is ValueBinding.ExternFunction -> {
                TODO()
            }
            is ValueBinding.FunctionParam -> {
                val index = binding.declaration.params.indexOfFirst {
                    it.binder.identifier.name == variable.name.name
                }
                val ty = ctx.checker.typeOfBinding(binding)
                assert(index > -1)
                IRParam(ty, nameOfGlobalBinder(binding.declaration.name), index)
            }
            is ValueBinding.ImportAs -> TODO()
            is ValueBinding.ValBinding ->
                TODO()
//        builder.buildLoad(
//            localVariables[binding.statement.binder.location] ?: TODO("Unbound"),
//            generateUniqueName()
//        )
            is ValueBinding.Struct ->
                TODO()
//        llvmModule.getFunction(mangleQualifiedName(binding.qualifiedName))
//            ?: throw AssertionError(
//                "Function ${binding.qualifiedName} hasn't been added to llvm module"
//            )
        }

    private fun lowerCall(expression: Expression.Call): IRExpression {
        val callee = lowerExpression(expression.callee)
        val args = expression.args.map { lowerExpression(it.expression) }
        val type = typeOfExpression(expression)
        // TODO: Handle generic calls
        return builder.buildCall(type, callee, listOf(), args)
    }

    private fun typeOfExpression(expression: Expression): Type {
        return ctx.checker.typeOfExpression(expression)
    }

    private fun lowerStatement(statement: Statement) {
        TODO()
    }


    private fun nameOfGlobalBinder(name: Binder): IRGlobalName {
        val sourceFile = ctx.getSourceFileOf(name)
        val ty = ctx.checker.typeOfBinder(name)
        return IRGlobalName(ty, sourceFile.moduleName.append(name.identifier.name))
    }


    private fun nameOfGlobalIdent(ty: Type, ident: Identifier): IRGlobalName {
        return IRGlobalName(ty, QualifiedName(listOf(ident.name)))
    }


}
