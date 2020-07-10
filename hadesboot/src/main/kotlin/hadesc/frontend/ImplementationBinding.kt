package hadesc.frontend

import hadesc.ast.Declaration
import hadesc.ast.InterfaceRef

sealed class ImplementationBinding {
    abstract val interfaceRef: InterfaceRef
    data class GlobalImpl(
        val implDef: Declaration.Implementation
    ) : ImplementationBinding() {
        override val interfaceRef: InterfaceRef
            get() = implDef.interfaceRef
    }

    data class TypeBound(
        override val interfaceRef: InterfaceRef,
        val functionDef: Declaration.FunctionDef,
        val typeParamIndex: Int
    ) : ImplementationBinding()

    data class ImplParamTypeBound(
            override val interfaceRef: InterfaceRef,
            val implDef: Declaration.Implementation,
            val typeParamIndex: Int
    ) : ImplementationBinding()
}