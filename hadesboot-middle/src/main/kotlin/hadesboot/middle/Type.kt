package hadesboot.middle

sealed interface Type {
    data class Int(val width: Width, val sign: Sign): Type
    data class Tuple(val members: List<Type>) : Type

    enum class Width {
        W32,
        Size,
    }
    enum class Sign {
        Signed,
        Unsigned
    }
    companion object {
        val usize = Int(Width.Size, Sign.Unsigned)
        val isize = Int(Width.Size, Sign.Signed)
        val u32 = Int(Width.W32, Sign.Unsigned)
        val i32 = Int(Width.W32, Sign.Signed)
    }
}