package hades
package languageserver

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, Reader}

import cats.Applicative
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import hades.languageserver.lsp.LSPRequestParams.{Exit, Initialize}
import hades.languageserver.lsp.{LSPRequest, LSPRequestParams, LSPResponse, LSPResponseParams, ServerCapabilities, ServerInfo}

import scala.io.StdIn

object LanguageServerMain extends IOApp {
  val reader = new BufferedReader(new InputStreamReader(System.in))
  val writer = new BufferedWriter(new OutputStreamWriter(System.out))
  implicit class ReaderOps[F[_]](val inputStream: F[InputStreamReader]) {}
  override def run(args: List[String]): IO[ExitCode] =
    loop

  def readContentLength: IO[Int] = for {
    line <- IO(StdIn.readLine())
    _ <- IO(StdIn.readLine())
    text = line.split(":")(1).trim
    length = text.toInt
  } yield length

  def log(message: String): IO[Unit] = IO(System.err.println(s"INFO: $message"))
  def putb(b: Int): IO[Unit] = IO(System.err.print(b.toChar))

  def readJSON(reader: BufferedReader): IO[String] = IO {
    var c = reader.read()
    val builder = new StringBuilder
    if (c.toChar != '{') {
      throw new AssertionError(s"Expected { found ${c} (${c.toChar})")
    }
    while (c.toChar != '}') {
      builder.addOne(c.toChar)
      c = reader.read()
    }
    builder.toString
  }

  def dump_c(reader: Reader): IO[Unit] = IO {
    val c = reader.read()
    System.err.println(s"ch: ${c} ${c.toChar}")
  }

  def loop: IO[ExitCode] = for {
    _ <- log("Waiting for request")
    header <- IO(reader.readLine())
    _ <- IO(reader.readLine())
    contentLength = header.split(":")(1).trim.toInt
    _ <- log(s"content length: ${contentLength}")
    buffer <- IO(new Array[Char](contentLength))
    _ <- IO(reader.read(buffer, 0, contentLength))
    jsonText = new String(buffer)
    _ <- log(jsonText)
    request = LSPRequest.fromJson(jsonText) match {
      case Right(value) => value
      case Left(e) => throw new RuntimeException(e)
    }
    _ <- log(request.toString)
    response <- handleRequest[IO](request)
    responseJson = response.toJsonString
    _ <- log(s"output: $responseJson")
    _ <- IO(writer.write(s"Content-Length: ${responseJson.length}\r\n\r\n"))
    _ <- IO(writer.write(responseJson))
    _ <- IO(writer.flush())
    code <- loop
  } yield code

  def handleInitializeRequest[F[_]: Applicative](request: LSPRequest, params: Initialize): F[LSPResponse] =
    LSPResponse(id = request.id, params = LSPResponseParams.Initialized(
      capabilities = ServerCapabilities(),
      serverInfo = Some(ServerInfo(
        name = "hades-language-server",
        version = None
      ))

    )).pure[F]

  def handleShutdownRequest[F[_]: Applicative](request: LSPRequest): F[LSPResponse] =
    LSPResponse(id=request.id, params = LSPResponseParams.Shutdown()).pure[F]

  def handleRequest[F[_]: Applicative: Sync](request: LSPRequest): F[LSPResponse] = {
    request.params match {
      case i: Initialize => handleInitializeRequest[F](request, i)
      case LSPRequestParams.Shutdown => handleShutdownRequest[F](request)
      case Exit =>
        System.exit(0)
        throw new RuntimeException("Unreachable")
    }
  }
}
