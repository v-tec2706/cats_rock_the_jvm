package utils

import cats.Functor
import cats.syntax.functor._

package object general {
  implicit class DebugWrapper[F[_], A](fa: F[A]) {
    def debug2(implicit functor: Functor[F]): F[A] = fa.map { a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }
  }
}
