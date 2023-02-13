package llvm

import org.bytedeco.javacpp.Pointer
import org.bytedeco.javacpp.PointerPointer
import org.bytedeco.llvm.global.LLVM
import kotlin.contracts.InvocationKind
import kotlin.contracts.contract
import kotlin.experimental.ExperimentalTypeInference

inline fun <reified T : Pointer> List<T>.asPointerPointer(): PointerPointer<T> {
    @Suppress("SpreadOperator")
    return PointerPointer(*this.toTypedArray())
}

fun Boolean.toLLVMBool(): Int {
    return if (this) 1 else 0
}

@Suppress("EXPERIMENTAL_IS_NOT_ENABLED")
@OptIn(ExperimentalTypeInference::class, kotlin.contracts.ExperimentalContracts::class)
inline fun <reified T> makeList(@BuilderInference builder: MutableList<T>.() -> Unit): List<T> {
    contract {
        callsInPlace(builder, InvocationKind.EXACTLY_ONCE)
    }
    val list = mutableListOf<T>()

    list.builder()

    return list
}

object LLVMUtils {
    @JvmStatic
    fun lookupIntrinsicID(name: String): Int {
        return LLVM.LLVMLookupIntrinsicID(name, name.length.toLong())
    }
}
