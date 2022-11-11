package hadesc.location

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import java.nio.file.Path

@Serializable(with = SourcePathSerializer::class)
data class SourcePath(
    val path: Path
) {
    override fun toString(): String {
        return path.toString()
    }
}

object SourcePathSerializer: KSerializer<SourcePath> {
    override val descriptor: SerialDescriptor
        get() = PrimitiveSerialDescriptor("SourcePath", kind = PrimitiveKind.STRING)

    override fun deserialize(decoder: Decoder): SourcePath {
        return SourcePath(Path.of(decoder.decodeString()))
    }

    override fun serialize(encoder: Encoder, value: SourcePath) {
        encoder.encodeString(value.toString())
    }

}