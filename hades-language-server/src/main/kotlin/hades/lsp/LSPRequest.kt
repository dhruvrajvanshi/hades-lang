package hades.lsp

import hades.json.decode
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.SerialKind
import kotlinx.serialization.descriptors.buildSerialDescriptor
import kotlinx.serialization.descriptors.element
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*
import hades.lsp.LSPRequestParams.*
import java.lang.UnsupportedOperationException

@Serializable(with = LSPRequestSerializer::class)
data class LSPRequest(
    val id: Long?,
    val params: LSPRequestParams
)

sealed class LSPRequestParams {
    @Serializable
    data class Initialize(
        /**
         * The process Id of the parent process that started
         * the server. Is null if the process has not been started by another process.
         * If the parent process is not alive then the server should exit (see exit notification) its process.
         */
        val processId: Int?,
        /**
         * The rootPath of the workspace. Is null
         * if no folder is open.
         *
         * @deprecated in favour of rootUri.
         */
        val rootPath: String?,
        /**
         * The rootUri of the workspace. Is null if no
         * folder is open. If both `rootPath` and `rootUri` are set
         * `rootUri` wins.
         */
        val rootUri: URI?,
        /**
         * User provided initialization options.
         */
//    initializationOptions: Option[AnyVal],
        /**
         * The capabilities provided by the client (editor or tool)
         */
        val capabilities: ClientCapabilities,
        /**
         * The initial trace setting. If omitted trace is disabled ('off').
         */
        val trace: String?,
        /**
         * The workspace folders configured in the client when the server starts.
         * This property is only available if the client supports workspace folders.
         * It can be `null` if the client supports workspace folders but none are
         * configured.
         *
         * Since 3.6.0
         */
        val workspaceFolders: List<WorkspaceFolder>?
    ) : LSPRequestParams() {
        companion object {
            @JvmStatic
            val METHOD: String = "initialize"
        }
    }

    @Serializable
    data class TextDocumentDidOpen(
        val textDocument: TextDocumentItem
    ) : LSPRequestParams() {
        companion object {
            @JvmStatic
            val METHOD = "textDocument/didOpen"
        }
    }

    @Serializable
    data class TextDocumentDidChange(
        val textDocument: VersionedTextDocumentIdentifier,
        val contentChanges: List<TextDocumentContentChangeEvent>
    ) : LSPRequestParams() {
        companion object {
            @JvmStatic
            val METHOD = "textDocument/didChange"
        }
    }


    @Serializable
    data class TextDocumentHover(
        val textDocument: TextDocumentIdentifier,
        val position: Position
    ) : LSPRequestParams() {

        companion object {
            @JvmStatic
            val METHOD = "textDocument/didHover"
        }
    }


    @Serializable
    object Shutdown : LSPRequestParams() {
        @JvmStatic
        val METHOD = "shutdown"
    }

    @Serializable
    object Exit : LSPRequestParams() {
        @JvmStatic
        val METHOD = "exit"
    }

    @Serializable
    object Initialized : LSPRequestParams() {
        @JvmStatic
        val METHOD = "initialized"
    }

    @Serializable
    data class Cancel(val id: Long) : LSPRequestParams() {
        companion object {
            @JvmStatic
            val METHOD = "$/cancelRequest"
        }
    }

    @Serializable
    data class Unknown(val method: String, val params: JsonElement?) : LSPRequestParams()
}

@Serializable
data class ClientInfo(
    val name: String,
    val version: String
)

@Serializable
data class WorkspaceFolder(
    val uri: URI,
    val name: String
)

@Serializable
data class ClientCapabilities(
    val workspace: WorkspaceClientCapabilities,
    val textDocument: TextDocumentClientCapabilities,
//  experimental: Option[AnyVal]
)

@Serializable
object WorkspaceClientCapabilities

@Serializable
object TextDocumentClientCapabilities

@Serializable
data class TextDocumentItem(
    val uri: URI,
    val languageId: String,
    val version: Int,
    val text: String
)

object LSPRequestSerializer : KSerializer<LSPRequest> {
    @OptIn(InternalSerializationApi::class)
    override val descriptor: SerialDescriptor
        get() = buildSerialDescriptor("LSPRequest", kind = SerialKind.CONTEXTUAL) {
            element<Long>("id")
            element<String>("method")
            element<LSPRequestParams>("params")
        }

    override fun deserialize(decoder: Decoder): LSPRequest {
        require(decoder is JsonDecoder)
        val elem = decoder.decodeJsonElement()
        require(elem is JsonObject)

        val id = elem["id"]

        require(id == null || id is JsonPrimitive)

        val idVal = if (id == null) {
            null
        } else {
            require(id is JsonPrimitive)
            id.long
        }

        val methodJ = elem["method"]
        require(methodJ is JsonPrimitive)
        require(methodJ.isString)

        val paramsJson = elem["params"]

        fun params(): JsonElement {
            return requireNotNull(paramsJson)
        }

        val params = when (methodJ.content) {
            Initialize.METHOD -> params().decode<Initialize>()
            Initialized.METHOD -> params().decode<Initialized>()
            TextDocumentDidOpen.METHOD -> params().decode<TextDocumentDidOpen>()
            TextDocumentDidChange.METHOD -> params().decode<TextDocumentDidChange>()
            TextDocumentHover.METHOD -> params().decode<TextDocumentHover>()
            Shutdown.METHOD -> Shutdown
            Exit.METHOD -> Exit
            Cancel.METHOD -> params().decode<Cancel>()
            else -> Unknown(methodJ.content, paramsJson)
        }
        return LSPRequest(
            id = idVal,
            params = params
        )
    }

    override fun serialize(encoder: Encoder, value: LSPRequest) =
        throw UnsupportedOperationException()
}

@Serializable(with = URISerializer::class)
data class URI(val uri: String)
object URISerializer : KSerializer<URI> {
    override fun deserialize(decoder: Decoder): URI =
        URI(decoder.decodeString())

    override fun serialize(encoder: Encoder, value: URI) {
        encoder.encodeString(value.uri)
    }

    @OptIn(InternalSerializationApi::class)
    override val descriptor: SerialDescriptor
        get() = buildSerialDescriptor("URI", SerialKind.CONTEXTUAL) {}

}

@Serializable
data class TextDocumentIdentifier(
    val uri: URI
)

@Serializable
data class Position(
    val line: Long,
    val character: Long,
)

@Serializable
data class VersionedTextDocumentIdentifier(
    val uri: URI,
    val version: Int,
)

@Serializable
data class TextDocumentContentChangeEvent(
    val range: Range?,
    val text: String
)

@Serializable
data class Range(
    val start: Position,
    val end: Position
)


