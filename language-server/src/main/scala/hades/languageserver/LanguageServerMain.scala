package hades
package languageserver

import java.io.InputStreamReader

import cats.effect.{ExitCode, IO, IOApp}
import hades.languageserver.logging.LoggerF
import hades.languageserver.parsing.ParsingContextIndex


object LanguageServerMain extends IOApp {

  implicit class ReaderOps[F[_]](val inputStream: F[InputStreamReader]) {}
  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.pure(())
    logger <- LoggerF.make[IO]
    parsingContextIndex <- ParsingContextIndex.build[IO]
    eventLoop = new EventLoop(
      parsingContextIndex = parsingContextIndex,
      logger = logger
    )
    code <- eventLoop.loop
  } yield code

}
