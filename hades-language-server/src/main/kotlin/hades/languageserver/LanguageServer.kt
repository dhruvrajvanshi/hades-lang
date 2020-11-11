package hades.languageserver

import arrow.core.Either
import arrow.core.Nel
import arrow.core.computations.either
import arrow.core.extensions.either.applicativeError.raiseError
import arrow.core.nel
import hades.json.decodeJson
import hades.lsp.LSPRequest
import hades.lsp.LSPRequestParams
import hades.lsp.LSPRequestParams.*
import java.util.*
import kotlin.system.exitProcess

class LanguageServer {
    private suspend fun log(message: String) {
        System.err.println(message)
    }

    private suspend fun debug(message: String) {
        System.err.println("DEBUG: $message")
    }

    suspend fun loop() {
        while (true) {
            when (val request = readRequest()) {
                is Either.Left -> {
                    debug("Error while trying to parse LSP request")
                    for (error in request.a) {
                        debug(error)
                    }
                }
                is Either.Right -> {
                    handleRequest(request.b)
                }
            }


        }
    }

    private suspend fun readRequest(): Either<Nel<String>, LSPRequest> = either {
        debug("Waiting for message")

        val header = readLSPHeader().bind()
        val contentLength = Either.fromNullable(header["content-length"]?.toIntOrNull())
            .mapLeft { "LSPHeader does not have a content length".nel() }
            .bind()
        val json = String(System.`in`.readNBytes(contentLength))
        json.decodeJson()
    }

    private suspend fun handleRequest(request: LSPRequest) {

        val exhaustive = when (request.params) {
            is Initialize -> handleInitialize(request, request.params)
            is TextDocumentDidOpen -> handleTextDocumentDidOpen(request, request.params)
            is TextDocumentDidChange -> handleTextDocumentDidChange(request, request.params)
            is TextDocumentHover -> handleHover(request, request.params)
            is Shutdown -> handleShutdown(request, request.params)
            is Exit -> handleExit(request, request.params)
            is Initialized -> handleInitialized(request, request.params)
            is Cancel -> handleCancel(request, request.params)
            is Unknown -> handleUnknown(request, request.params)
        }

        debug("got message: $request")

    }

    private fun handleCancel(request: LSPRequest, params: Cancel) {
        // Empty
    }

    private suspend fun handleUnknown(request: LSPRequest, params: Unknown) {
        debug("Unhandled message $params")
    }

    private fun handleExit(request: LSPRequest, params: Exit) {
        exitProcess(0)
    }

    private fun handleShutdown(request: LSPRequest, params: Shutdown) {
        // Start cleaning up resources here
    }

    private fun handleHover(request: LSPRequest, params: TextDocumentHover) {
        TODO()
    }

    private fun handleInitialized(request: LSPRequest, params: Initialized) {
        // Nothing
    }

    private fun handleTextDocumentDidChange(request: LSPRequest, params: TextDocumentDidChange) {
        TODO()
    }

    private suspend fun handleTextDocumentDidOpen(request: LSPRequest, params: LSPRequestParams.TextDocumentDidOpen) {
        debug("Text document did open")
    }

    private suspend fun handleInitialize(
        request: LSPRequest, params: LSPRequestParams.Initialize
    )  {
        debug("Received initialize request")
        val json = """
            {
              "id": ${request.id},
              "result": {
                "capabilities": {
                  "textDocumentSync": {
                    "openClose": true,
                    "change": 2
                  },
                  "hoverProvider": {
                    "workDoneProgress": true
                  }
                },
                "serverInfo": {
                  "name": "hades-language-server",
                  "version": null
                }
              }
            }
        """.trimIndent()
        System.out.print("Content-Length: ${json.length}\r\n")
        System.out.print("\r\n")
        System.out.print(json)
    }

    private suspend fun readLSPHeader(): Either<Nel<String>, LSPHeader> = either {
        val errors = mutableListOf<String>()
        val header = LSPHeader.build {
            while (true) {
                val line = readLine()
                if (line == null || line == "") {
                    return@build
                }
                if (line.count { it == ':' } != 1) {
                    errors.add("Invalid header '$line': Must contain exactly one ':'")
                }
                val split = line.split(':', limit = 2)
                put(split[0].trim(), split[1].trim())
            }
        }
        if (errors.isNotEmpty()) {
            Nel.fromListUnsafe(errors)
                .raiseError<Nel<String>, LSPHeader>().bind()
        } else {
            header
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
        fun build(builder: TreeMap<String, String>.() -> Unit): LSPHeader {
            val m = TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER)
            m.builder()
            return LSPHeader(m)
        }
    }
}