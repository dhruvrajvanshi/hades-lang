package hades.languageserver

import hades.json.decodeJson
import hades.lsp.LSPRequest
import kotlinx.serialization.json.Json
import java.lang.RuntimeException
import java.lang.UnsupportedOperationException
import java.util.*

class LanguageServer {
    suspend fun log(message: String) {
        System.err.println(message)
    }

    suspend fun debug(message: String) {
        System.err.println("DEBUG: $message")
    }

    suspend fun loop() {
        while (true) {
            debug("Waiting for message")
            val header = readLSPHeader()
            val contentLength = header["content-length"]?.toIntOrNull()
            if (contentLength == null) {
                log("Invalid LSP header. Does not contain a content-length; Ignoring")
                continue
            }
            val json = String(System.`in`.readNBytes(contentLength))

            val request = json.decodeJson<LSPRequest>()

            handleRequest(request)

        }
    }

    private suspend fun handleRequest(request: LSPRequest) {

        debug("got message: $request")

        System.out.println("""
                {
                  "id": 0,
                  "params": {
                    "capabilities": {
                      "textDocumentSync": {
                        "openClose": true,
                        "change": 2
                      },
                      "hoverProvider": {
                        "workDoneProgress": true
                      }
                    ),
                    "serverInfo": {
                      "name": "hades-language-server",
                      "version": null
                    }
                  }
                }
            """.trimIndent())

    }

    private suspend fun readLSPHeader() = LSPHeader.build {
        while (true) {
            val line = readLine()
            if (line == null || line == "") {
                return@build
            }
            val split = line.split(':', limit = 2)
            assert(split.size == 2)
            put(split[0].trim(), split[1].trim())
        }
    }
}

class LSPHeader private constructor(
    private val map: TreeMap<String, String>
) {
    operator fun get(key: String): String? = map[key]

    override fun toString(): String {
        return map.toString()
    }

    companion object {
        public fun build(builder: TreeMap<String, String>.() -> Unit): LSPHeader {
            val m = TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER)
            m.builder()
            return LSPHeader(m)
        }
    }
}