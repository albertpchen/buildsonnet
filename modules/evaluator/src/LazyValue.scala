package buildsonnet.evaluator

import cats.{Applicative, Monad}
import cats.effect.{Deferred, Concurrent, Ref}
import cats.syntax.all.given

opaque type LazyValue[F[_]] = F[EvaluatedJValue[F]]

object LazyValue:
  def strict[F[_]: Applicative](evaluated: EvaluatedJValue[F]): LazyValue[F] =
    evaluated.pure

  def apply[F[_]: Concurrent: Ref.Make](code: F[EvaluatedJValue[F]]): F[LazyValue[F]] =
    for
      lockRef <- Ref[F].of(false)
      deferred <- Deferred[F, EvaluatedJValue[F]]
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

  extension [F[_]](lazyVal: LazyValue[F])
    def value: F[EvaluatedJValue[F]] = lazyVal



final class LazyObjectValue[F[_]] private (
  val isHidden: Boolean,
  val value: F[EvaluatedJValue[F]],
)

object LazyObjectValue:
  def apply[F[_]: Concurrent: Monad: Ref.Make](isHidden: Boolean, code: F[EvaluatedJValue[F]]): F[LazyObjectValue[F]] =
    LazyValue[F](code).map(lazyVal => new LazyObjectValue(isHidden, lazyVal.value))

/*

sealed trait LazyValue:
  def withContext(ctx: EvaluationContext): LazyValue
  def evaluated: EvaluatedJValue


private class LazyValueFromCode(
  ctx: EvaluationContext,
  code: JValue,
):
  def withContext(newCtx: EvaluationContext) =
    new LazyValueFromCode(newCtx, code)

  lazy val evaluated = eval(ctx)(code)

  override def toString = code.toString


private class LazyValueStrict(
  val evaluated: EvaluatedJValue
):
  def withContext(newCtx: EvaluationContext) = this

  override def toString = evaluated.toString


object LazyValue:
  def apply(ctx: EvaluationContext, code: JValue): LazyValue =
    new LazyValueFromCode(ctx, code)

  def strict(value: EvaluatedJValue): LazyValue =
    new LazyValueStrict(value)



sealed trait LazyObjectValue extends LazyValue:
  override def withContext(ctx: EvaluationContext): LazyObjectValue
  val isHidden: Boolean
  def withHidden(hidden: Boolean): LazyObjectValue


private class LazyObjectValueFromCode(
  ctx: EvaluationContext,
  code: JValue,
  val isHidden: Boolean,
) extends LazyValueFromCode(ctx, code):

  def withContext(newCtx: EvaluationContext) =
    new LazyObjectValueFromCode(newCtx, code, isHidden)

  def withHidden(hidden: Boolean) =
    new LazyObjectValueFromCode(ctx, code, isHidden)


private class LazyObjectValueStrict(
  value: EvaluatedJValue,
  val isHidden: Boolean,
) extends LazyValueStrict(value):


object LazyObjectValue:
  def apply(ctx: EvaluationContext, code: JValue, isHidden: Boolean): LazyObjectValue =
    new LazyObjectValueFromCode(ctx, code, isHidden)

  def strict(value: EvaluatedJValue): LazyObjectValue =
    new LazyObjectValueStrict(value)
*/
