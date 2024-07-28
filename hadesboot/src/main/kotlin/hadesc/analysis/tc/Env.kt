package hadesc.analysis.tc

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.Declaration
import hadesc.ast.SourceFile
import hadesc.ast.TypeAnnotation
import hadesc.resolver.Binding
import hadesc.resolver.NewResolver
import hadesc.types.Type

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

    companion object {
        @JvmStatic
        val empty = Empty

        @JvmStatic
        fun ofSourceFile(
            file: SourceFile,
            resolver: NewResolver,
            lower: TypeAnnotation.() -> Type,
            parent: Env?,
        ): Env {
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
                    Type.ForAll(
                        typeParams.map { Type.Param(it.binder) },
                        fnPtrType,
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
            for (decl in file.declarations) {
                when (decl) {
                    is Declaration.ConstDefinition -> {
                        values[decl.name.name] = decl.annotation?.lower() ?: Type.Error(decl.location, "Type annotation is required here")
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
                                    values[binding.declaration.name.name] =
                                        binding.declaration.annotation?.lower() ?:
                                            Type.Error(
                                                binding.declaration.location,
                                                "Type annotation required"
                                            )
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
    }
}
