package hadesc.analysis

import hadesc.ast.Binder
import hadesc.types.Type

class TypeAnalyzer {
    private val genericInstances = mutableMapOf<Long, Type>()

    fun getInstantiatedType(type: Type.GenericInstance): Type? {
        return genericInstances[type.id]
    }

    fun isTypeAssignableTo(source: Type, destination: Type): Boolean {
        if (source == destination) {
            return true
        }
        return when {
            source is Type.Error -> true
            destination is Type.Error -> true
            destination is Type.GenericInstance -> {
                val existing = genericInstances[destination.id]
                if (existing != null) {
                    isTypeAssignableTo(source, destination = existing)
                } else {
                    genericInstances[destination.id] = source
                    true
                }
            }
            source is Type.GenericInstance -> {
                val existing = genericInstances[source.id]
                if (existing != null) {
                    isTypeAssignableTo(existing, destination = destination)
                } else {
                    genericInstances[source.id] = destination
                    true
                }
            }
            destination is Type.Application && source is Type.Application -> {
                isTypeAssignableTo(source.callee, destination.callee) &&
                    source.args.size == destination.args.size &&
                    source.args.zip(destination.args).all {
                        isTypeAssignableTo(source = it.first, destination = it.second)
                    }
            }

            destination is Type.Param && source is Type.Param -> {
                destination.name.id == source.name.id
            }
            destination is Type.Ptr && source is Type.Ptr -> {
                val ptrTypeAssignable = isTypeAssignableTo(source.to, destination.to)
                if (destination.isMutable && !source.isMutable) {
                    false
                } else {
                    ptrTypeAssignable
                }
            }
            destination is Type.Constructor && source is Type.Constructor -> {
                destination.name == source.name
            }
            destination is Type.FunctionPtr && source is Type.FunctionPtr -> {
                destination.from.size == source.from.size &&
                    isTypeAssignableTo(source = source.to, destination = destination.to) &&
                    source.from.zip(destination.from).all { (sourceParam, destParam) ->
                        isTypeAssignableTo(source = destParam, destination = sourceParam)
                    }
            }
            destination is Type.Closure && source is Type.Closure -> {
                destination.from.size == source.from.size &&
                    isTypeAssignableTo(source = source.to, destination = destination.to) &&
                    source.from.zip(destination.from).all { (sourceParam, destParam) ->
                        isTypeAssignableTo(source = destParam, destination = sourceParam)
                    }
            }
            else -> false
        }
    }

    private var makeGenericInstanceId = 0L
    fun makeGenericInstance(binder: Binder): Type.GenericInstance {
        val id = makeGenericInstanceId
        makeGenericInstanceId++
        return Type.GenericInstance(
            binder.name,
            binder.location,
            id = id
        )
    }
}
