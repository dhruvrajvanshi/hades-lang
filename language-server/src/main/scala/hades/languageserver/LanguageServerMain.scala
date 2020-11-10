package hades
package languageserver

import cats.effect.{ExitCode, IO, IOApp}
import hades.languageserver.parsing.ParsingContextIndex
import hades.languageserver.reader.InputStreamReaderF


object LanguageServerMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.pure(())
    parsingContextIndex <- ParsingContextIndex.build[IO]
    reader <- InputStreamReaderF.build[IO](System.in)
    eventLoop <- EventLoop.build(
      parsingContextIndex = parsingContextIndex,
      inputStreamReaderF = reader
    )
    code <- eventLoop.loop
  } yield code

}
