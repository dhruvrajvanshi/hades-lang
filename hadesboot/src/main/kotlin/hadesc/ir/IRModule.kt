package hadesc.ir

import hadesc.Name
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

class IRModule {
    private val definitions = mutableListOf<IRDefinition>()
    private val globals = mutableMapOf<QualifiedName, IRBinding>()
    val size get() = definitions.size
    fun prettyPrint(): String = definitions.joinToString("\n") { it.prettyPrint() }

    fun addExternFunctionDef(
        location: SourceLocation,
        name: IRGlobalName,
        type: Type.Function,
        externName: Name
    ): IRExternFunctionDef {
        val value = IRExternFunctionDef(this, location, name, type, externName = externName)
        add(value)
        return value
    }

    fun addExternConstDef(location: SourceLocation, name: IRGlobalName, type: Type, externName: Name): IRExternConstDef {
        val value = IRExternConstDef(this, location, name, type, externName = externName)
        add(value)
        return value
    }

    fun addConstDef(
            location: SourceLocation,
            name: IRGlobalName,
            type: Type,
            initializer: IRValue
    ): IRConstDef {
        val def = IRConstDef(this, location, name, type, initializer)
        add(def)
        return def
    }

    fun addGlobalFunctionDef(
            location: SourceLocation,
            name: IRGlobalName,
            type: Type.Function,
            typeParams: List<IRTypeParam>?,
            params: List<IRParam>,
            entryBlock: IRBlock,
            constraints: List<IRConstraint>
    ): IRFunctionDef {
        val value = IRFunctionDef(
                this,
                location,
                IRFunctionSignature(
                        location,
                        name,
                        type,
                        typeParams,
                        params,
                        constraints
                ),
                entryBlock,
                blocks = mutableListOf())
        add(value)
        return value
    }

    fun addStructDef(
            location: SourceLocation,
            constructorType: Type.Function,
            instanceType: Type,
            name: IRGlobalName,
            typeParams: List<IRTypeParam>?,
            fields: Map<Name, Type>
    ): IRStructDef {
        val value = IRStructDef(
                this, location, constructorType, instanceType, name, typeParams, fields)
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
            is IRInterfaceDef -> TODO()
            is IRImplementationDef -> TODO()
            is IRExternConstDef -> globals[def.name.name] = IRBinding.ExternConstDef(def)
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