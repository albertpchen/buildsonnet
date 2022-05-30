package buildsonnet.evaluator

import cats.{Applicative, Monad}
import cats.effect.{Deferred, Concurrent, Ref}
import cats.syntax.all.given

opaque type LazyValue[F[_]] = F[EvaluatedJValue[F]]

object LazyValue:
  def strict[F[_]: Applicative](evaluated: EvaluatedJValue[F]): LazyValue[F] =
    evaluated.pure

  def memoize[F[_]: Concurrent: Ref.Make, T](code: F[T]): F[F[T]] =
    for
      lockRef <- Ref[F].of(false)
      deferred <- Deferred[F, T]
    yield
      for
        lock <- lockRef.getAndSet(true)
        value <-
          if lock then
            deferred.get
          else
            code.flatMap(value => deferred.complete(value).as(value))
      yield
        value


  def apply[F[_]: Concurrent: Ref.Make](code: F[EvaluatedJValue[F]]): F[LazyValue[F]] =
    memoize(code)

  extension [F[_]](lazyVal: LazyValue[F])
    def value: F[EvaluatedJValue[F]] = lazyVal



final class LazyObjectValue[F[_]] private (
  val isHidden: Boolean,
  val value: F[EvaluatedJValue[F]],
)

object LazyObjectValue:
  def apply[F[_]: Concurrent: Monad: Ref.Make](isHidden: Boolean, code: F[EvaluatedJValue[F]]): F[LazyObjectValue[F]] =
    LazyValue[F](code).map(lazyVal => new LazyObjectValue(isHidden, lazyVal.value))

  def strict[F[_]: Applicative](isHidden: Boolean, value: EvaluatedJValue[F]): LazyObjectValue[F] =
    new LazyObjectValue(isHidden, value.pure)
