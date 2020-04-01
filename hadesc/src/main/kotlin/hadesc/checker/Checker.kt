package hadesc.checker

import hadesc.Name
import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.Identifier
import hadesc.ast.TypeAnnotation
import hadesc.context.Context
import hadesc.resolver.TypeBinding
import hadesc.resolver.ValueBinding
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class Checker(val ctx: Context) {
    fun annotationToType(annotation: TypeAnnotation): Type {
        return when (annotation.kind) {
            TypeAnnotation.Kind.Error -> Type.Error
            is TypeAnnotation.Kind.Var -> when (identToString(annotation.kind.name)) {
                // TODO: Should be able to override built in types?
                // is it a good idea?
                "Byte" -> Type.Byte
                "Void" -> Type.Void
                "Bool" -> Type.Bool
                else -> when (val typeBinding = ctx.resolver.getTypeBinding(annotation.kind.name)) {
                    is TypeBinding.FunctionDefTypeParam -> Type.ParamRef(
                        typeBinding.binder,
                        typeBinding.paramIndex
                    )
                }
            }
            is TypeAnnotation.Kind.Ptr -> Type.RawPtr(annotationToType(annotation.kind.to))
        }
    }

    fun typeError() = Type.Error

    fun typeOfExpression(expression: Expression): Type = when (expression.kind) {
        Expression.Kind.Error -> typeError()
        is Expression.Kind.Var -> typeOfBinding(ctx.resolver.getBinding(expression.kind.name))
        is Expression.Kind.Call -> {
            when (val funcType = typeOfExpression(expression.kind.callee)) {
                is Type.Function -> funcType.to
                else -> typeError()
            }
        }
        is Expression.Kind.Property -> {
            val lhsType = typeOfExpression(expression.kind.lhs)
            typeOfProperty(lhsType, expression.kind.property)
        }
        is Expression.Kind.ByteString -> Type.RawPtr(Type.Byte)
        is Expression.Kind.BoolLiteral -> Type.Bool
    }

    fun typeOfProperty(type: Type, propertyName: Identifier): Type = when (type) {
        is Type.Struct -> {
            type.memberTypes[propertyName.name] ?: TODO("${propertyName.location}: No such property")
        }
        is Type.ModuleAlias -> {
            typeOfBinding(ctx.resolver.resolveModuleMember(type.qualifiedName, propertyName))
        }
        else -> {
            TODO("${propertyName.location}: No such property on type $type")
        }
    }

    fun typeOfBinding(binding: ValueBinding): Type = when (binding) {
        is ValueBinding.GlobalFunction -> {
            if (binding.kind.typeParams.isNotEmpty()) {
                Type.GenericFunction(
                    typeParams = binding.kind.typeParams,
                    from = binding.kind.params.map {
                        annotationToType(it.annotation ?: TODO("Type annotation required"))
                    },
                    to = annotationToType(binding.kind.returnType)
                )
            } else {
                Type.Function(
                    from = binding.kind.params.map {
                        annotationToType(it.annotation ?: TODO("Type annotation required"))
                    },
                    to = annotationToType(binding.kind.returnType)
                )
            }
        }
        is ValueBinding.ExternFunction -> Type.Function(
            from = binding.kind.paramTypes.map { annotationToType(it) },
            to = annotationToType(binding.kind.returnType)
        )
        is ValueBinding.FunctionParam -> annotationToType(binding.param.annotation ?: TODO("Annotation required"))
        is ValueBinding.ImportAs -> {
            Type.ModuleAlias(binding.aliasedModule)
        }
        is ValueBinding.ValBinding -> typeOfExpression(binding.kind.rhs)
        is ValueBinding.Struct -> typeOfStructConstructor(binding.kind)
    }

    private fun identToString(identifier: Identifier): String {
        return identifier.name.text
    }

    fun typeOfStructInstance(kind: Declaration.Kind.Struct): Type {
        val name = ctx.resolver.getBinding(kind.binder.identifier).qualifiedName
        val memberTypes = mutableMapOf<Name, Type>()
        for (member in kind.members) {
            val exhaustive = when (member) {
                Declaration.Kind.Struct.Member.Error -> {
                }
                is Declaration.Kind.Struct.Member.Field -> {
                    val type = annotationToType(member.typeAnnotation)
                    memberTypes[member.binder.identifier.name] = type
                    Unit
                }
            }
        }
        return Type.Struct(
            name,
            memberTypes
        )
    }

    fun typeOfStructConstructor(kind: Declaration.Kind.Struct): Type {

        val memberTypeList = buildList {
            kind.members.forEach {
                if (it is Declaration.Kind.Struct.Member.Field) {
                    add(annotationToType(it.typeAnnotation))
                }
            }
        }
        return Type.Function(from = memberTypeList, to = typeOfStructInstance(kind))
    }

    fun checkDeclaration(declaration: Declaration) = when (declaration.kind) {
        Declaration.Kind.Error -> {
        }
        is Declaration.Kind.ImportAs -> checkImportAsDeclaration(declaration, declaration.kind)
        is Declaration.Kind.FunctionDef -> checkFunctionDef(declaration, declaration.kind)
        is Declaration.Kind.ExternFunctionDef -> checkExternFunctionDef(declaration, declaration.kind)
        is Declaration.Kind.Struct -> checkStructDeclaration(declaration, declaration.kind)
    }

    private fun checkStructDeclaration(declaration: Declaration, kind: Declaration.Kind.Struct) {
        // TODO
    }

    private fun checkExternFunctionDef(declaration: Declaration, kind: Declaration.Kind.ExternFunctionDef) {
        // TODO
    }

    private fun checkFunctionDef(declaration: Declaration, kind: Declaration.Kind.FunctionDef) {
        for (param in kind.params) {
            if (param.annotation != null) {
                annotationToType(param.annotation)
            }
        }
        // TODO
    }

    private fun checkImportAsDeclaration(declaration: Declaration, kind: Declaration.Kind.ImportAs) {
        // TODO
    }

    fun isGenericCallSite(kind: Expression.Kind.Call): Boolean {
        val ty = typeOfExpression(kind.callee)
        return ty is Type.GenericFunction
    }
}