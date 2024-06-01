package hadesboot.middle

fun Module.prettyPrint(): String =
    items.joinToString("\n\n") { it.prettyPrint() }

fun Item.prettyPrint(): String = when (this) {
    is ExternFn -> {
        val params = parameters.joinToString(", ") { it.prettyPrint() }
        "fn $name($params) -> ${returnType.prettyPrint()}"
    }
    is Fn -> {
        val params = parameters.joinToString(", ") { it.prettyPrint() }
        "fn $name($params) -> ${returnType.prettyPrint()}"
    }
}

fun Type.prettyPrint(): String = when(this) {
    is Type.Int -> {
        val signS = when(sign) {
            Type.Sign.Signed -> "i"
            Type.Sign.Unsigned -> "u"
        }
        val width = when(width) {
            Type.Width.W32 -> "32"
            Type.Width.Size -> "size"
        }
        signS + width
    }
    is Type.Tuple ->
            members.joinToString( ", ", "(",  ")") { it.prettyPrint() }
}

fun Parameter.prettyPrint(): String = "$name: ${type.prettyPrint()}"