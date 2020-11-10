package hades
package languageserver

import cats.effect.{ExitCode, IO, IOApp}
import hades.languageserver.lsp.LSPResponseWriter
import hades.languageserver.parsing.ParsingContextIndex
import hades.languageserver.reader.InputStreamReaderF
import hades.languageserver.writer.OutputStreamWriterF

object LanguageServerMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.pure(())
    parsingContextIndex <- ParsingContextIndex.build[IO]
    reader <- InputStreamReaderF.build[IO](System.in)
    outStreamWriter <- OutputStreamWriterF.build[IO](System.out)
    writer <- LSPResponseWriter.build[IO](outStreamWriter)
    eventLoop <- EventLoop.build(
      parsingContextIndex = parsingContextIndex,
      reader = reader,
      writer = writer
    )
    code <- eventLoop.loop
  } yield code

}
