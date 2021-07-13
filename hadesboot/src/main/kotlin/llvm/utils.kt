package llvm

import org.bytedeco.javacpp.Pointer
import org.bytedeco.javacpp.PointerPointer

inline fun <reified T: Pointer> List<T>.asPointerPointer(): PointerPointer<T> {
    return PointerPointer(*this.toTypedArray())
}

fun Boolean.toLLVMBool(): Int {
    return if (this) 1 else 0
}

inline fun <reified T> makeList(builder: MutableList<T>.() -> Unit): List<T> {
    val list = mutableListOf<T>()

    list.builder()

    return list
}