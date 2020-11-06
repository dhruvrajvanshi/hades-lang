package hades
package languageserver

import java.io.{BufferedReader, InputStreamReader, Reader}

import cats.effect.{ExitCode, IO, IOApp}
import hades.languageserver.lsp.LSPRequest
import hades.languageserver.lsp.LSPRequest.lspRequestDecoder
import io.circe.parser.decode

import scala.io.StdIn

object LanguageServerMain extends IOApp {
  val reader = new BufferedReader(new InputStreamReader(System.in))
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
    request = decode[LSPRequest](jsonText)
    _ <- log(request.toString)
    _ <- IO(throw new UnsupportedOperationException)
    code <- loop
  } yield code

}
