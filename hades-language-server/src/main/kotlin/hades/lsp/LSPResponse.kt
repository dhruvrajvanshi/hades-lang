package hades.lsp


data class LSPResponse(
    val id: Int,
    val params: Params
) {
    companion object {
        sealed class Params {


            data class Initialized(
                /**
                 * The capabilities the language server provides.
                 */
                val capabilities: Any,
                /**
                 * Information about the server.
                 *
                 * @since 3.15.0
                 */
                val serverInfo: ServerInfo?
            ): Params()

            object Shutdown: Params()

            data class Hover(val contents: String): Params()
        }
    }
}


object ServerCapabilities

data class ServerInfo(
    /**
     * The name of the server as defined by the server.
     */
    val name: String,
    /**
     * The server's version as defined by the server.
     */
    val version: String?
)

sealed class DeclarationProviderOptions {
    object DeclarationOptions: DeclarationProviderOptions()
    object DeclarationRegistrationOptions: DeclarationProviderOptions()
}
