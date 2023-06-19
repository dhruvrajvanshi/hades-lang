package mir

import java.nio.file.Path
sealed interface MIRValue {
    val type: MIRType

    data class I32(
        val value: Int
    ): MIRValue {
        override val type get(): MIRType.I32 = MIRType.I32
    }
    data class LocalRef(
        override val type: MIRType,
        val name: String,
    ): MIRValue

}
