package hades.languageserver.lsp

import io.circe.generic._
import io.circe._
import io.circe.syntax._

@JsonCodec sealed abstract class LSPRequest



object LSPRequest {

  @JsonCodec case class Initialize(
    /**
     * The process Id of the parent process that started
     * the server. Is null if the process has not been started by another process.
     * If the parent process is not alive then the server should exit (see exit notification) its process.
     */
    processId: Option[Int],
    /**
     * The rootPath of the workspace. Is null
     * if no folder is open.
     *
     * @deprecated in favour of rootUri.
     */
    rootPath: Option[String],
    /**
     * The rootUri of the workspace. Is null if no
     * folder is open. If both `rootPath` and `rootUri` are set
     * `rootUri` wins.
     */
    rootUri: Option[String],
    /**
     * User provided initialization options.
     */
//    initializationOptions: Option[AnyVal],
    /**
     * The capabilities provided by the client (editor or tool)
     */
    capabilities: ClientCapabilities,
    /**
     * The initial trace setting. If omitted trace is disabled ('off').
     */
    trace: Option[String],
    /**
     * The workspace folders configured in the client when the server starts.
     * This property is only available if the client supports workspace folders.
     * It can be `null` if the client supports workspace folders but none are
     * configured.
     *
     * Since 3.6.0
     */
    workspaceFolders: Option[List[WorkspaceFolder]]
  ) extends LSPRequest
  object Initialize {
    val METHOD = "initialize"
  }

  val KEY_PARAMS = "params"
  val KEY_METHOD = "method"
  implicit val lspRequestEncoder: Encoder[LSPRequest] = {
    case i: Initialize => Json.obj(
      (KEY_METHOD, Initialize.METHOD.asJson),
      (KEY_PARAMS, i.asJson)
    )
  }

  implicit val lspRequestDecoder: Decoder[LSPRequest] = json => for {
    t <- json.downField(KEY_METHOD).as[String]
    payload <- t match {
      case Initialize.METHOD => Decoder[Initialize].tryDecode(json.downField(KEY_PARAMS))
      case _ => Left(DecodingFailure(s"Unknown message type $t", List()))
    }
  } yield payload
}

@JsonCodec case class ClientInfo(
  name: String,
  version: String
)

@JsonCodec case class WorkspaceFolder(
  uri: String,
  name: String
)

@JsonCodec case class ClientCapabilities(
  workspace: Option[WorkspaceClientCapabilities],
  textDocument: Option[TextDocumentClientCapabilities],
//  experimental: Option[AnyVal]
)

@JsonCodec case class WorkspaceClientCapabilities()
@JsonCodec case class TextDocumentClientCapabilities()

