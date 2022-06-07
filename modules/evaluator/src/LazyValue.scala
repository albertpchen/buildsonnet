package buildsonnet.evaluator

import cats.{Applicative, Monad}
import cats.effect.{Deferred, Concurrent, Ref}
import cats.effect.syntax.all.given
import cats.syntax.all.given

opaque type LazyValue[F[_]] = F[EvaluatedJValue[F]]

object LazyValue:
  def strict[F[_]: Applicative](evaluated: EvaluatedJValue[F]): LazyValue[F] =
    evaluated.pure

  def apply[F[_]: Concurrent: Ref.Make](code: F[EvaluatedJValue[F]]): F[LazyValue[F]] =
    code.memoize

  extension [F[_]](lazyVal: LazyValue[F]) def value: F[EvaluatedJValue[F]] = lazyVal

final class LazyObjectValue[F[_]] private (
  val isHidden: Boolean,
  val value: F[EvaluatedJValue[F]],
)

object LazyObjectValue:
  def apply[F[_]: Concurrent: Monad: Ref.Make](
    isHidden: Boolean,
    code: F[EvaluatedJValue[F]],
  ): F[LazyObjectValue[F]] =
    LazyValue[F](code).map(lazyVal => new LazyObjectValue(isHidden, lazyVal.value))

  def strict[F[_]: Applicative](isHidden: Boolean, value: EvaluatedJValue[F]): LazyObjectValue[F] =
    new LazyObjectValue(isHidden, value.pure)
