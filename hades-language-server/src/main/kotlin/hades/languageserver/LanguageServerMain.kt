package hades.languageserver

import hades.languageserver.logging.logger
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

class LanguageServerMain {
    companion object {
        @JvmStatic
        private val log = logger()
        @JvmStatic
        fun main(args: Array<String>) {
            log.info("Starting hades-language-server")
            val server = HadesLSPServer()
            val launcher = Launcher.Builder<LanguageClient>()
                .setLocalService(server)
                .setInput(System.`in`)
                .setOutput(System.out)
                .setRemoteInterface(LanguageClient::class.java)
                .create()
            val client = launcher.remoteProxy
            server.connect(client)
            launcher.startListening().get()
        }
    }

}
