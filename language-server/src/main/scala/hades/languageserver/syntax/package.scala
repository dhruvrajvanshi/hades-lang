package hades.languageserver

import cats.Applicative

package object syntax {
  implicit class ApplicativeOps[T](value: T) {
    def pure[F[_]: Applicative](): F[T] = implicitly[Applicative[F]].pure(value)
  }
}
