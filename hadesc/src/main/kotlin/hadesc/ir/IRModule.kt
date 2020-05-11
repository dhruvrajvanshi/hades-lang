package hadesc.ir

import hadesc.Name
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

class IRModule {
    private val definitions = mutableListOf<IRDefinition>()
    private val globals = mutableMapOf<QualifiedName, IRBinding>()
    fun prettyPrint(): String = definitions.joinToString("\n") { it.prettyPrint() }

    fun addExternFunctionDef(
            name: IRGlobalName,
            type: Type.Function,
            externName: Name
    ): IRExternFunctionDef {
        val value = IRExternFunctionDef(this, name, type, externName = externName)
        add(value)
        return value
    }

    fun addConstDef(
            name: IRGlobalName,
            type: Type,
            initializer: IRValue
    ): IRConstDef {
        val def = IRConstDef(this, name, type, initializer)
        add(def)
        return def
    }

    fun addGlobalFunctionDef(
            name: IRGlobalName,
            type: Type.Function,
            typeParams: List<IRTypeParam>?,
            params: List<IRParam>,
            entryBlock: IRBlock
    ): IRFunctionDef {
        val value = IRFunctionDef(this, name, type, typeParams, params, entryBlock, blocks = mutableListOf())
        add(value)
        return value
    }

    fun addStructDef(
            constructorType: Type.Function,
            instanceType: Type,
            name: IRGlobalName,
            typeParams: List<IRTypeParam>?,
            fields: Map<Name, Type>
    ): IRStructDef {
        val value = IRStructDef(this, constructorType, instanceType, name, typeParams, fields)
        add(value)
        return value

    }

    operator fun iterator(): Iterator<IRDefinition> = definitions.iterator()

    fun add(def: IRDefinition) {
        definitions.add(def)
        return when (def) {
            is IRFunctionDef -> globals[def.name.name] = IRBinding.FunctionDef(def)
            is IRStructDef -> globals[def.globalName.name] = IRBinding.StructDef(def)
            is IRExternFunctionDef -> globals[def.name.name] = IRBinding.ExternFunctionDef(def)
            is IRConstDef -> globals[def.name.name] = IRBinding.ConstDef(def)
        }
    }

    fun resolveGlobal(name: IRGlobalName): IRBinding {
        return resolveGlobal(name.name)
    }

    fun resolveGlobal(name: QualifiedName): IRBinding {
        return requireNotNull(globals[name]) {
            "Global ${name.mangle()} not present in module"
        }
    }
}