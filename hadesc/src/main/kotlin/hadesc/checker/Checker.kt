package hadesc.checker

import hadesc.Name
import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.Identifier
import hadesc.ast.TypeAnnotation
import hadesc.context.Context
import hadesc.resolver.ValueBinding
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class Checker(val ctx: Context) {
    fun annotationToType(annotation: TypeAnnotation): Type {
        return when (annotation.kind) {
            TypeAnnotation.Kind.Error -> Type.Void
            is TypeAnnotation.Kind.Var -> when (identToString(annotation.kind.name)) {
                // TODO: Use scoped lookup here
                "Byte" -> Type.Byte
                "Void" -> Type.Void
                else -> TODO("Unbound type variable ${annotation.kind.name.name.text}")
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
    }

    fun typeOfProperty(type: Type, propertyName: Identifier): Type = when (type) {
        is Type.Struct -> {
            type.memberTypes[propertyName.name] ?: TODO("${propertyName.location}: No such property")
        }
        else -> {
            TODO("${propertyName.location}: No such property on type $type")
        }
    }

    fun typeOfBinding(binding: ValueBinding): Type = when (binding) {
        is ValueBinding.GlobalFunction -> TODO()
        is ValueBinding.ExternFunction -> TODO()
        is ValueBinding.FunctionParam -> TODO()
        is ValueBinding.ImportAs -> TODO()
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
}