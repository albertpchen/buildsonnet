package buildsonnet.evaluator

import cats.{Applicative, Eval, Foldable, Traverse}
import cats.data.Chain
import cats.syntax.all.given

object IArrayInstances:
  given Traverse[IArray] with
    def foldLeft[A, B](fa: IArray[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    def foldRight[A, B](fa: IArray[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable.iterateRight(fa, lb)(f)

    def traverse[G[_], A, B](fa: IArray[A])(f: (A) => G[B])(implicit
      G: Applicative[G],
    ): G[IArray[B]] = ???
