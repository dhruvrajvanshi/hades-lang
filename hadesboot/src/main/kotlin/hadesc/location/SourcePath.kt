package hadesc.location

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import java.net.URI
import java.nio.file.Path

@Serializable(with = SourcePathToURISerializer::class)
data class SourcePath(
    val path: Path
) {
    override fun toString(): String {
        return path.toString()
    }
}

private class SourcePathToURISerializer : KSerializer<SourcePath> {
    override val descriptor = PrimitiveSerialDescriptor("SourcePath", PrimitiveKind.STRING)

    override fun deserialize(decoder: Decoder): SourcePath =
        SourcePath(Path.of(URI.create(decoder.decodeString())))

    override fun serialize(encoder: Encoder, value: SourcePath) =
        encoder.encodeString(value.path.toUri().toString())
}
