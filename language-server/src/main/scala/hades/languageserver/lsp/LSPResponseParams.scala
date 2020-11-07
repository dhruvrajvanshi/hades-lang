package hades.languageserver.lsp

import hades.languageserver.lsp.LSPResponseParams.{Hover, Initialized, Shutdown}
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
    "result" -> self.params._asJson
  )
}

sealed abstract class LSPResponseParams {

  def _asJson: Json = this match {
    case i: Initialized => i.asJson
    case _: Shutdown => Json.obj()
    case h: Hover => h.asJson
  }
}

object LSPResponseParams {

  @JsonCodec case class Initialized(
    /**
     * The capabilities the language server provides.
     */
    capabilities: Json,
    /**
     * Information about the server.
     *
     * @since 3.15.0
     */
    serverInfo: Option[ServerInfo]
  ) extends LSPResponseParams

  @JsonCodec case class Shutdown() extends LSPResponseParams

  @JsonCodec case class Hover(contents: String) extends LSPResponseParams
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

sealed trait DeclarationProviderOptions
object DeclarationProviderOptions {
  case class DeclarationOptions() extends DeclarationProviderOptions
  case class DeclarationRegistrationOptions() extends DeclarationProviderOptions
}
