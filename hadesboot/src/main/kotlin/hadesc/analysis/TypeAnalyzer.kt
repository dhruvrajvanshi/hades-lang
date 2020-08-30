package hadesc.analysis

import hadesc.ast.Binder
import hadesc.location.SourceLocation
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
                isTypeAssignableTo(source.callee, destination.callee)
                        && source.args.size == destination.args.size
                        && source.args.zip(destination.args).all {
                    isTypeAssignableTo(source = it.first, destination = it.second)
                }
            }

            destination is Type.ParamRef && source is Type.ParamRef -> {
                destination.name.identifier.name == source.name.identifier.name
                        && destination.name.location == source.name.location
            }
            destination is Type.Ptr && source is Type.Ptr -> {
                val ptrTypeAssignable = isTypeAssignableTo(source.to, destination.to)
                if (destination.isMutable && !source.isMutable) {
                    false
                } else {
                    ptrTypeAssignable
                }

            }
            destination is Type.Application && source is Type.Application -> {
                isTypeAssignableTo(source = source.callee, destination = destination.callee)
                        && source.args.zip(destination.args).all {
                    isTypeAssignableTo(source = it.first, destination = it.second)
                }
            }
            destination is Type.Constructor && source is Type.Constructor -> {
                destination.name == source.name
            }
            destination is Type.Function && source is Type.Function -> {
                destination.from.size == source.from.size
                        && isTypeAssignableTo(source = source.to, destination = destination.to)
                        && source.from.zip(destination.from).all { (sourceParam, destParam) ->
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
                binder,
                id = id)
    }

    fun instantiate(implType: Type, params: List<Type.Param>): Type {
        val substitution = params.map { it.binder.location to makeGenericInstance(it.binder) }.toMap()
        return implType.applySubstitution(substitution)
    }

}