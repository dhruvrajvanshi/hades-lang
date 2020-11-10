package hades.languageserver

import java.io._

import cats.Applicative
import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import hades.languageserver.logging.LoggerF
import hades.languageserver.lsp.LSPRequestParams.Initialize
import hades.languageserver.lsp.LSPResponseParams.Hover
import hades.languageserver.lsp._
import hades.languageserver.parsing.ParsingContextIndex
import io.circe.Json
import io.circe.syntax._

import scala.io.StdIn

class EventLoop(
  val parsingContextIndex: ParsingContextIndex[IO],
  val logger: LoggerF[IO]
) {
  type F[T] = IO[T]
  val reader = new BufferedReader(new InputStreamReader(System.in))
  val writer = new BufferedWriter(new OutputStreamWriter(System.out))

  def loop: IO[ExitCode] = for {
    _ <- log("Waiting for request")
    header <- IO(reader.readLine())
    _ <- IO(reader.readLine())
    contentLength = header.split(":")(1).trim.toInt
    _ <- log(s"content length: $contentLength")
    buffer <- IO(new Array[Char](contentLength))
    _ <- IO(reader.read(buffer, 0, contentLength))
    jsonText = new String(buffer)
    _ <- log(jsonText)
    request = LSPRequest.fromJson(jsonText) match {
      case Right(value) => value
      case Left(e)      => throw new RuntimeException(e)
    }
    response <- handleRequest(jsonText, request)
    _ <- log("responding with " + response.toString)
    responseJson = response.map(_.toJsonString)
    _ <- log(s"output: $responseJson")
    _ <- responseJson match {
      case Some(responseJson) =>
        for {
          _ <- IO(writer.write(s"Content-Length: ${responseJson.length}\r\n\r\n"))
          _ <- IO(writer.write(responseJson))
          _ <- IO(writer.flush())
        } yield ()
      case None => ().pure[IO]
    }
    code <- loop
  } yield code

  def handleShutdownRequest[F[_]: Applicative](request: LSPRequest): F[LSPResponse] =
    LSPResponse(id = request.id, params = LSPResponseParams.Shutdown()).pure[F]

  def handleTextDocumentHover[F[_]: Applicative](request: LSPRequest,
                                                 h: LSPRequestParams.TextDocumentHover
  ): F[Some[LSPResponse]] =
    Some(
      LSPResponse(
        params = Hover("hover not implemented"),
        id = request.id
      )
    ).pure[F]

  def handleTextDocumentDidChange[F[_]: Applicative](request: LSPRequest,
                                                     c: LSPRequestParams.TextDocumentDidChange
  ): F[Option[LSPResponse]] =
    None.asInstanceOf[Option[LSPResponse]].pure[F]

  def handleRequest(reqJSON: String, request: LSPRequest): IO[Option[LSPResponse]] = {
    import LSPRequestParams._
    request.params match {
      case i: Initialize            => handleInitializeRequest[IO](request, i).map(Some(_))
      case Shutdown                 => handleShutdownRequest[IO](request).map(Some(_))
      case Initialized              => None.pure[IO]
      case o: TextDocumentDidOpen   => handleTextDocumentDidOpen(o)
      case h: TextDocumentHover     => handleTextDocumentHover[IO](request, h)
      case c: TextDocumentDidChange => handleTextDocumentDidChange[IO](request, c)
      case Unknown =>
        for {
          _ <- log(s"WARN: Unhandled method ${io.circe.parser.decode[Json](reqJSON).map(_.spaces2)}")
        } yield None
      case Exit =>
        System.exit(0)
        throw new RuntimeException("Unreachable")
    }
  }

  def handleTextDocumentDidOpen(open: LSPRequestParams.TextDocumentDidOpen): IO[Option[LSPResponse]] =
    parsingContextIndex.onDocumentOpen(open) >> None.asInstanceOf[Option[LSPResponse]].pure[F]

  private def readContentLength: IO[Int] = for {
    line <- IO(StdIn.readLine())
    _ <- IO(StdIn.readLine())
    text = line.split(":")(1).trim
    length = text.toInt
  } yield length

  private def log(message: String): IO[Unit] =
    logger.info(message)

  private def readJSON(reader: BufferedReader): IO[String] = IO {
    var c = reader.read()
    val builder = new StringBuilder
    if (c.toChar != '{') {
      throw new AssertionError(s"Expected { found $c (${c.toChar})")
    }
    while (c.toChar != '}') {
      builder.addOne(c.toChar)
      c = reader.read()
    }
    builder.toString
  }

  private def dump_c(reader: Reader): IO[Unit] = IO {
    val c = reader.read()
    System.err.println(s"ch: $c ${c.toChar}")
  }

  private def handleInitializeRequest[F[_]: Applicative](request: LSPRequest, params: Initialize): F[LSPResponse] =
    LSPResponse(
      id = request.id,
      params = LSPResponseParams.Initialized(
        capabilities = Json.obj(
          "textDocumentSync" -> Json.obj(
            "openClose" -> true.asJson,
            "change" -> 2.asJson
          ),
          "hoverProvider" -> Json.obj(
            "workDoneProgress" -> true.asJson
          )
        ),
        serverInfo = Some(
          ServerInfo(
            name = "hades-language-server",
            version = None
          )
        )
      )
    ).pure[F]
}
