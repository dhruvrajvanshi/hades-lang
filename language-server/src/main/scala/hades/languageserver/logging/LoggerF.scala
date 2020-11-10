package hades.languageserver.logging

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}

trait LoggerF[F[_]] {
  def info(message: String): F[Unit]
  def warn(message: String): F[Unit]
  def debug(message: String): F[Unit]

  def profile[T](name: String)(f: => F[T]): F[T]
}

object LoggerF {
  def build[F[_]: Monad: Sync, C](clazz: Class[C]): F[LoggerF[F]] = {
    new LoggerFImpl[F, C](clazz).asInstanceOf[LoggerF[F]].pure[F]
  }
}

private class LoggerFImpl[F[_]: Applicative: Sync, C](val clazz: Class[C]) extends LoggerF[F] {
  override def info(message: String): F[Unit] = withTag("INFO", message)

  private def withTag(tag: String, message: String): F[Unit] = Sync[F].delay {
    System.err.println(s"$tag (${clazz.getName}): $message")
  }

  override def warn(message: String): F[Unit] = withTag("WARN", message)

  override def debug(message: String): F[Unit] = withTag("DEBUG", message)

  override def profile[T](name: String)(f: => F[T]): F[T] = for {
    startTime <- Sync[F].delay(System.currentTimeMillis())
    value <- f
    stopTime <- Sync[F].delay(System.currentTimeMillis())
    _ <- withTag("PROFILE", s"$name took ${stopTime - startTime}ms to execute")
  } yield value

}
