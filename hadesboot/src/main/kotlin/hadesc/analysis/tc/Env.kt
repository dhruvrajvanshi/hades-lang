package hadesc.analysis.tc

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.assertions.requireUnreachable
import hadesc.ast.Declaration
import hadesc.ast.Expression
import hadesc.ast.SourceFile
import hadesc.ast.TypeAnnotation
import hadesc.resolver.Binding
import hadesc.resolver.NewResolver
import hadesc.types.Type
import hadesc.types.ptr

fun interface TypeLower {
    operator fun invoke(typeAnnotation: TypeAnnotation): Type
}

sealed interface Env {
    val parent: Env?

    data object Empty: Env {
        override val parent get() = null
    }
    data class Scope(
        override val parent: Env?,
        val values: Map<Name, Type>,
        val enumDeclarations: Map<Name, Declaration.Enum>,
    ): Env
    data class Let(
        override val parent: Env,
        val name: Name,
        val type: Type,
    ): Env

    fun resolveValue(name: Name): Type? = when(this) {
        Empty -> null
        is Scope -> values[name] ?: parent?.resolveValue(name)
        is Let ->
            if (name == this.name)
                type
            else
                parent.resolveValue(name)
    }

    companion object {
        @JvmStatic
        val empty = Empty

        @JvmStatic
        fun ofSourceFile(
            file: SourceFile,
            resolver: NewResolver,
            lowerType: TypeLower,
            parent: Env?,
        ): Env {
            fun TypeAnnotation.lower(): Type = lowerType(this)

            fun Declaration.ExternFunctionDef.type(): Type.FunctionPtr {
                val paramTypes = paramTypes.map { it.lower() }
                val returnType = returnType.lower()
                return Type.FunctionPtr(
                    from = paramTypes,
                    to = returnType,
                )
            }

            fun Declaration.FunctionDef.type(): Type {
                val fnPtrType = Type.FunctionPtr(
                    from = params.map { it.annotation?.lower() ?: Type.Error(it.location, "Missing type annotation") },
                    to = signature.returnType.lower()
                )
                return if (typeParams != null) {
                    val requirements = signature.whereClause?.traitRequirements?.mapNotNull { requirement ->
                        val declaration = resolver.resolveDeclaration(requirement.path)
                        val args = requirement.typeArgs?.map { it.lower() }
                        if (declaration !is Declaration.TraitDef) {
                            null
                        } else {
                            TraitRequirement(resolver.qualifiedName(declaration.name), args ?: listOf(), negated = requirement.negated)
                        }
                    }
                    Type.ForAll(
                        typeParams.map { Type.Param(it.binder) },
                        fnPtrType,
                        requirements = requirements ?: emptyList(),
                    )
                } else {
                    fnPtrType
                }
            }

            fun Declaration.Struct.constructorType(): Type {
                var instanceType: Type = Type.Constructor(resolver.qualifiedName(binder))
                if (typeParams != null) {
                    instanceType = Type.Application(instanceType, typeParams.map { Type.Param(it.binder) })
                }
                val fnPtrType = Type.FunctionPtr(
                    from = members.map {
                        when (it) {
                            is Declaration.Struct.Member.Field -> {
                                it.typeAnnotation.lower()
                            }
                        }
                    },
                    to = instanceType
                )
                return if (typeParams != null) {
                    Type.ForAll(
                        typeParams.map { Type.Param(it.binder) },
                        fnPtrType,
                    )
                } else {
                    fnPtrType
                }
            }
            val values = mutableMapOf<Name, Type>()
            val enumDecls = mutableMapOf<Name, Declaration.Enum>()
            fun Declaration.ConstDefinition.type(): Type {
                if (annotation != null) {
                    return annotation.lower()
                }
                return when (initializer) {
                    is Expression.Var -> {
                        values[initializer.name.name] ?: Type.Error(location, "Type annotation is required here")
                    }
                    is Expression.IntLiteral -> Type.i32
                    is Expression.BoolLiteral -> Type.Bool
                    is Expression.CString -> Type.CChar.ptr()
                    is Expression.FloatLiteral -> Type.f64
                    else ->
                        Type.Error(location, "Type annotation is required here")
                }
            }

            for (decl in file.declarations) {
                when (decl) {
                    is Declaration.ConstDefinition -> {
                        values[decl.name.name] = decl.type()
                    }
                    is Declaration.Enum -> {
                        enumDecls[decl.name.name] = decl
                    }
                    is Declaration.Error -> Unit
                    is Declaration.ExtensionDef -> Unit
                    is Declaration.ExternConst -> {
                        values[decl.name.name] = decl.type.lower()
                    }
                    is Declaration.ExternFunctionDef -> {
                        values[decl.binder.name] = decl.type()
                    }
                    is Declaration.FunctionDef -> {
                        values[decl.name.name] = decl.type()
                    }
                    is Declaration.ImplementationDef -> Unit
                    is Declaration.ImportAs -> Unit
                    is Declaration.ImportMembers -> {
                        val sourceFile = resolver.getSourceFile(decl.modulePath) ?: continue
                        for (name in decl.names) {
                            val binding = resolver.findInSourceFile(name.name, sourceFile) ?: continue
                            when (binding) {
                                // Unreachable because import foo.bar.{baz} can only refer to global definitions
                                is Binding.ClosureParam -> requireUnreachable()
                                is Binding.MatchArmEnumCaseArg -> requireUnreachable()
                                is Binding.ValBinding -> requireUnreachable()
                                is Binding.FunctionParam -> requireUnreachable()
                                is Binding.Enum -> {
                                    enumDecls[binding.declaration.name.name] = binding.declaration
                                }
                                is Binding.ExternConst -> {
                                    values[binding.declaration.name.name] = binding.declaration.type.lower()
                                }
                                is Binding.ExternFunction -> {
                                    values[binding.declaration.binder.name] = binding.declaration.type()
                                }

                                is Binding.GlobalConst -> {
                                    values[binding.declaration.name.name] = binding.declaration.type()
                                }
                                is Binding.GlobalFunction -> {
                                    values[binding.declaration.name.name] = binding.declaration.type()
                                }
                                is Binding.Struct -> {
                                    values[binding.declaration.binder.name] = binding.declaration.constructorType()
                                }
                            }
                        }
                    }

                    is Declaration.Struct -> {
                        values[decl.binder.name] = decl.constructorType()
                    }
                    is Declaration.TraitDef -> Unit
                    is Declaration.TypeAlias -> {
                        // Adds nothing to the environment because
                        // type aliases should already be collapsed
                        // by [TypeChecker.lower]
                    }
                }
            }
            return Scope(parent, values, enumDecls)
        }

        fun ofFunction(
            def: Declaration.FunctionDef,
            parent: Env,
            lowerType: TypeLower,
        ): Env {
            fun TypeAnnotation.lower(): Type = lowerType(this)

            return Scope(
                parent = parent,
                values = def.params.associate {
                    it.binder.name to (it.annotation?.lower() ?: Type.Error(it.location, "Missing type annotation"))
                },
                enumDeclarations = emptyMap(),
            )
        }
    }
}
