package hades.languageserver

import java.io.{BufferedReader, InputStreamReader, Reader}
import java.util.concurrent.{Executors, TimeUnit}

import cats.{Applicative, Monad}
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import cats.syntax._
import cats.effect._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn

object LanguageServerMain extends IOApp {
  implicit class ReaderOps[F[_]](val inputStream: F[InputStreamReader]) {

  }
  override def run(args: List[String]): IO[ExitCode] =
    loop

  override def contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.fromExecutor(Executors.newWorkStealingPool(4)))

  def loop: IO[ExitCode] = for {
    line <- IO(StdIn.readLine())
    _ <- IO(Console.err.println(s"DEBUG: $line"))
    code <- if (line == "exit")
      ExitCode.Success.pure[IO]
    else loop
  } yield code


}

