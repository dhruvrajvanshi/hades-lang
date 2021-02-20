package hadesc.hir

import hadesc.location.HasLocation

interface HIRNode : HasLocation {

    fun prettyPrint(): String
}