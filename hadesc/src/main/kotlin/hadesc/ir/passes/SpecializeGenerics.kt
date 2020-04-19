package hadesc.ir.passes

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.ir.*
import hadesc.types.Type

class SpecializeGenerics(val ctx: Context, val module: IRModule) {
    val newModule = IRModule()

    fun run(): IRModule {
        return module
        for (def in module) {
            val toAdd = when (def) {
                is IRFunctionDef -> {
                    if (def.typeParams != null) {
                        def
                    } else {
                        def
                    }
                }
                is IRStructDef -> {
                    if (def.typeParams != null) {
                        null
                    } else {
                        def
                    }
                }
                is IRExternFunctionDef -> def
            }
            if (toAdd != null) {
                newModule.add(toAdd)
            }
        }
        for (definition in newModule) {
            val exhaustive = when (definition) {
                is IRFunctionDef -> visitFunctionDef(definition)
                is IRStructDef -> {
                }
                is IRExternFunctionDef -> {
                }
            }
        }
        return newModule
    }

    private fun visitFunctionDef(definition: IRFunctionDef): Unit {
        for (statement in definition.body) {
            val exhaustive = when (statement) {
                is IRValStatement -> visitExpression(statement.initializer)
                is IRReturnStatement -> visitExpression(statement.value)
                is IRReturnVoidStatement -> {
                }
                is IREmptyStatement -> {
                }
                is IRCall -> TODO()
            }
        }
    }

    private fun visitExpression(expression: IRValue) = when (expression) {
        is IRBool -> {
        }
        is IRByteString -> {
        }
        is IRVariable -> {
        }
        is IRGetStructField -> {
        }
    }

    private fun visitCallExpression(expression: IRCall) {
        for (arg in expression.args) {
            visitExpression(arg)
        }
        replaceCalleeWithSpecialization(expression.callee, expression.typeArgs)
    }

    private fun replaceCalleeWithSpecialization(callee: IRValue, typeArgs: List<Type>?) {
        if (typeArgs == null) {
            return
        }

        val newBinder = when (callee) {
            is IRBool -> requireUnreachable()
            is IRByteString -> requireUnreachable()
            is IRVariable -> {
                when (callee.binding) {
                    is IRBinding.FunctionDef -> createSpecializedFunctionDef(callee.binding.def, typeArgs)
                    is IRBinding.ExternFunctionDef -> requireUnreachable { "Extern functions cannot be generic" }
                    is IRBinding.ValStatement -> TODO()
                    is IRBinding.StructDef -> TODO()
                    is IRBinding.ParamRef -> TODO()
                    is IRBinding.CallStatement -> TODO()
                }
            }
            is IRGetStructField -> TODO()
        }
        val builder = IRBuilder()

    }

    private fun createSpecializedFunctionDef(def: IRFunctionDef, typeArgs: List<Type>): IRBinder {
        TODO()
    }

}
