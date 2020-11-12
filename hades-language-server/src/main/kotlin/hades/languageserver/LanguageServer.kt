package hades.languageserver

import arrow.core.*
import arrow.core.computations.either
import arrow.core.extensions.either.applicativeError.raiseError
import arrow.fx.asCoroutineContext
import hades.json.decodeJson
import hades.lsp.request.LSPRequest
import hades.lsp.request.LSPRequestParams
import hades.lsp.request.LSPRequestParams.*
import kotlinx.coroutines.*
import java.io.File
import java.util.*
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import kotlin.system.exitProcess

class LanguageServer {

    private val stdinContext = Executors.newSingleThreadExecutor(ThreadFactory {
        Thread(it, "stdin-context")
    }).asCoroutineContext()
    private val stdOutContext = Executors.newSingleThreadExecutor(ThreadFactory {
        Thread(it, "stdout-context")
    }).asCoroutineContext()

    private val stderrContext = Executors.newFixedThreadPool(5, ThreadFactory {
        Thread(it, "stderr-context")
    }).asCoroutineContext()

    private suspend fun debug(message: String) = withContext(stderrContext) {
        System.err.println("    DEBUG (${Thread.currentThread().name}-${Thread.currentThread().id})): $message")
        File("log.log").appendText("DEBUG $message\n")
    }

    @Volatile
    private var serverState = ServerState.NOT_STARTED
    private val serverStateContext = newSingleThreadContext("serverState")

    enum class ServerState {
        NOT_STARTED,
        INITIALIZED,
        SHUTDOWN_RECEIVED,
        EXIT_OK,
        EXIT_ERROR,

    }
    private val ServerState.isRunning get() = this != ServerState.EXIT_OK && this != ServerState.EXIT_ERROR

    suspend fun loop(scope: CoroutineScope): Unit = scope.run {
        while (serverState.isRunning) {
            debug("LOOP: $serverState (${serverState.isRunning})")
            when (val request = readRequest()) {
                is Either.Left -> {
                    debug("Error while trying to parse LSP request")
                    for (error in request.a) {
                        debug(error)
                    }
                }
                is Either.Right -> {
                    launch {
                        handleRequest(request.b)
                    }
                }
            }
            if (serverState == ServerState.SHUTDOWN_RECEIVED) {
                delay(1000)
            }
        }
        serverStateContext.close()
        require(serverState == ServerState.EXIT_ERROR || serverState == ServerState.EXIT_OK)
        if (serverState == ServerState.EXIT_OK) {
            exitProcess(0)
        } else {
            exitProcess(1)
        }
    }

    private suspend fun readNBytes(length: Int): ByteArray = withContext(stdinContext) {
        debug("readNBytes running in ${Thread.currentThread().name}")
        System.`in`.readNBytes(length)
    }
    private suspend fun readString(length: Int): String {
        return String(readNBytes(length))

    }

    private suspend fun write(text: String) = withContext(stdOutContext) {
        System.out.print(text)
        System.out.flush()
    }

    private suspend fun readRequest(): Either<Nel<String>, LSPRequest> = either {
        val header = ! readLSPHeader()
        val contentLength = ! header.contentLength
        val json = readString(contentLength)
        json.decodeJson()
    }

    private val LSPHeader.contentLength: Either<Nel<String>, Int> get() =
        Either.fromNullable(this["content-length"])
            .mapLeft { "LSPHeader does not have a content length ($this)".nel() }
            .flatMap {
                val i = it.toIntOrNull()
                if (i == null) {
                    "Expected content-length to be a number; Found '$it'".nel().left()
                } else if (i < 0) {
                    "Client send negative Content-Length ($i)".nel().left()
                } else {
                    i.right()
                }
            }

    private suspend fun handleRequest(request: LSPRequest) {
        debug("handling request in ${Thread.currentThread().name}")

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

    private suspend fun handleExit(request: LSPRequest, params: Exit) {
        if (serverState == ServerState.SHUTDOWN_RECEIVED) {
            setState(ServerState.EXIT_OK)
        } else {
            setState(ServerState.EXIT_ERROR)
        }
    }

    private suspend fun handleShutdown(request: LSPRequest, params: Shutdown) {
        val json = """
            {
                "id": ${request.id},
                "result": "shutdown"
            }
        """.trimIndent()
        write("Content-Length: ${json.length}\r\n")
        write("\r\n")
        write(json)
        setState(ServerState.SHUTDOWN_RECEIVED)
    }

    private suspend fun setState(state: ServerState) = withContext(serverStateContext) {
        debug("Updating server state $serverState -> $state")
        serverState = state
    }

    private fun handleHover(request: LSPRequest, params: TextDocumentHover) {
        // Nothing
    }

    private fun handleInitialized(request: LSPRequest, params: Initialized) {
        // Nothing
    }

    private fun handleTextDocumentDidChange(request: LSPRequest, params: TextDocumentDidChange) {
        // Nothing
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
        write("Content-Length: ${json.length}\r\n")
        write("\r\n")
        write(json)
        setState(ServerState.INITIALIZED)
    }

    private suspend fun readLSPHeader(): Either<Nel<String>, LSPHeader> = either {
        val errors = mutableListOf<String>()
        val header = LSPHeader.build {
            while (true) {
                val line = withContext(stdinContext) { readLine() }
                if (line == null || line == "") {
                    return@build
                }
                if (line.count { it == ':' } != 1) {
                    errors.add("Invalid header '$line': Must contain exactly one ':'")
                    return@build
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
        suspend fun build(builder: suspend TreeMap<String, String>.() -> Unit): LSPHeader {
            val m = TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER)
            m.builder()
            return LSPHeader(m)
        }
    }
}