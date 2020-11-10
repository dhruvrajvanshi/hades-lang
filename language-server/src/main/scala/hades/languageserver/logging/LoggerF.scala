package hades.languageserver.logging

import java.io.PrintStream

import cats.Applicative
import cats.effect.Sync
import cats.syntax.applicative._

trait LoggerF[F[_]] {
  def info(message: String): F[Unit]
}

object LoggerF {
  def make[F[+_]: Applicative: Sync]: F[LoggerF[F]] = {
    new PrintStreamLogger[F](System.err).pure[F]
  }
}

private class PrintStreamLogger[F[_]: Applicative: Sync](stream: PrintStream) extends LoggerF[F] {
  override def info(message: String): F[Unit] = Sync[F].delay[Unit] {
    stream.println(s"INFO: $message")
  }

}
