package hades
package languageserver

import java.io.InputStreamReader

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import hades.languageserver.parsing.{ParsingContextIndex, ParsingContextIndexImpl, State}


object LanguageServerMain extends IOApp {

  implicit class ReaderOps[F[_]](val inputStream: F[InputStreamReader]) {}
  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- IO.pure(())
    parsingState <- Ref[IO].of(State())
    parsingContextIndex = new ParsingContextIndexImpl[IO](parsingState)
    eventLoop = new EventLoop(parsingContextIndex = parsingContextIndex)
    code <- eventLoop.loop
  } yield code

}
