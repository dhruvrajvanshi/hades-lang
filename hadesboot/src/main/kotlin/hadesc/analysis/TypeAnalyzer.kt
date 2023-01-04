package hadesc.analysis

import hadesc.ast.Binder
import hadesc.types.Substitution
import hadesc.types.Type
import hadesc.types.toSubstitution

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
            source is Type.AssociatedTypeRef && destination is Type.AssociatedTypeRef -> {
                source.binder.location == destination.binder.location
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
            destination is Type.Constructor && source is Type.Constructor -> {
                destination.name == source.name
            }
            destination is Type.FunctionPtr && source is Type.FunctionPtr -> {
                destination.from.size == source.from.size
                        && isTypeAssignableTo(source = source.to, destination = destination.to)
                        && source.from.zip(destination.from).all { (sourceParam, destParam) ->
                    isTypeAssignableTo(source = destParam, destination = sourceParam)
                }
            }
            destination is Type.Closure && source is Type.Closure -> {
                destination.from.size == source.from.size
                        && isTypeAssignableTo(source = source.to, destination = destination.to)
                        && source.from.zip(destination.from).all { (sourceParam, destParam) ->
                    isTypeAssignableTo(source = destParam, destination = sourceParam)
                }
            }
            source is Type.Select && destination is Type.Select -> {
                source.associatedTypeName == destination.associatedTypeName
                        && source.traitName == destination.traitName
                        && source.traitArgs.size == destination.traitArgs.size
                        && source.traitArgs.zip(destination.traitArgs).all { (source, destination) ->
                            isTypeAssignableTo(source = source, destination = destination)
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
                id = id)
    }

    fun makeParamSubstitution(params: List<Type.Param>): Substitution {
        return params.associate { it.binder.location to makeGenericInstance(it.binder) }.toSubstitution()
    }

}