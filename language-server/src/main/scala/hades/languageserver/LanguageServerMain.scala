package hades
package languageserver

import java.io.InputStreamReader

import cats.effect.{ExitCode, IO, IOApp}
import hades.languageserver.parsing.ParsingContextIndex


object LanguageServerMain extends IOApp {

  implicit class ReaderOps[F[_]](val inputStream: F[InputStreamReader]) {}
  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.pure(())
    parsingContextIndex <- ParsingContextIndex.build[IO]
    eventLoop <- EventLoop.build(parsingContextIndex = parsingContextIndex)
    code <- eventLoop.loop
  } yield code

}
