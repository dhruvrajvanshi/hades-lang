package hades.languageserver.lsp

import io.circe._
import io.circe.generic._
import io.circe.syntax._

case class LSPResponse(
  id: Int,
  params: LSPResponseParams
) {

  def toJsonString: String =
    this.asJson.noSpaces
}
object LSPResponse {
  implicit val jsonEncoder: Encoder[LSPResponse] = self => Json.obj(
    "id" -> self.id.asJson,
    "result" -> self.params.asJson
  )
}

@JsonCodec sealed abstract class LSPResponseParams {
  def toJsonString: String =
    this.asJson.noSpaces
}

object LSPResponseParams {

  @JsonCodec case class Initialized(
    /**
     * The capabilities the language server provides.
     */
    capabilities: ServerCapabilities,
    /**
     * Information about the server.
     *
     * @since 3.15.0
     */
    serverInfo: Option[ServerInfo]
  ) extends LSPResponseParams

  @JsonCodec case class Shutdown() extends LSPResponseParams
}

@JsonCodec case class ServerCapabilities()

@JsonCodec case class ServerInfo(
  /**
   * The name of the server as defined by the server.
   */
  name: String,
  /**
   * The server's version as defined by the server.
   */
  version: Option[String]
)
