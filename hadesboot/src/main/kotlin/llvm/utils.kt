package llvm

import org.bytedeco.javacpp.Pointer
import org.bytedeco.javacpp.PointerPointer

inline fun <reified T: Pointer> List<T>.asPointerPointer(): PointerPointer<T> {
    return PointerPointer(*this.toTypedArray())
}

fun Boolean.toLLVMBool(): Int {
    return if (this) 1 else 0
}