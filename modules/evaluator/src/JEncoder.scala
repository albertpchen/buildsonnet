package buildsonnet.evaluator

import cats.{Monad, Parallel}
import cats.effect.Sync
import cats.syntax.all.given

import buildsonnet.ast.{Source}
import buildsonnet.evaluator.EvaluationContext

import scala.compiletime.{constValue, erasedValue, summonInline, summonFrom}
import scala.deriving.Mirror

import java.nio.file.Path


trait JEncoder[F[_], T]:
  type _T = T
  def encode(ctx: EvaluationContext[F], src: Source, t: T): EvaluatedJValue[F]

object JEncoder:
  def apply[F[_], T](using JEncoder[F, T]): JEncoder[F, T] = summon

  given [F[_]]: JEncoder[F, String] with
    def encode(ctx: EvaluationContext[F], src: Source, t: String) = EvaluatedJValue.JString[F](src, t)

  given [F[_]]: JEncoder[F, Boolean] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Boolean) = EvaluatedJValue.JBoolean[F](src, t)

  given [F[_]]: JEncoder[F, Double] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Double) = EvaluatedJValue.JNum[F](src, t)

  given [F[_]]: JEncoder[F, Int] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Int) = EvaluatedJValue.JNum[F](src, t)

  given [F[_]]: JEncoder[F, Path] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Path) =
      if t.getRoot == null then
        EvaluatedJValue.JString[F](src, t.toString)
      else
        EvaluatedJValue.JString[F](src, ctx.workspaceDir.relativize(t).toString)

  given [F[_], L[X] <: Iterable[X], T: [T] =>> JEncoder[F, T]]: JEncoder[F, L[T]] with
    def encode(ctx: EvaluationContext[F], src: Source, t: L[T]) =
      EvaluatedJValue.JArray[F](src, IArray.unsafeFromArray(t.map(JEncoder[F, T].encode(ctx, src, _)).toArray))

  given [F[_]: Sync, T: [T] =>> JEncoder[F, T]]: JEncoder[F, Map[String, T]] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Map[String, T]) =
      val members = t.map { (k, v) =>
        k -> JEncoder[F, T].encode(ctx, src, v)
      }
      EvaluatedJValue.JObject.static(src, members)

  inline given [F[_]: Sync, T: Mirror.ProductOf]: JEncoder[F, T] = derived[F, T]

  inline def summonAll[F[_], T <: Tuple]: List[JEncoder[F, ?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JEncoder[F, t]] :: summonAll[F, ts]

  inline def getLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].toString :: getLabels[ts]

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  inline def derived[F[_]: Sync, T](using m: Mirror.ProductOf[T]): JEncoder[F, T] =
    new JEncoder[F, T]:
      def encode(ctx: EvaluationContext[F], src: Source, t: T) =
        val values = iterator(t).zip(summonAll[F, m.MirroredElemTypes]).map { (t, _e) =>
          val e = _e // need a stable path
          e.encode(ctx, src, t.asInstanceOf[e._T])
        }
        EvaluatedJValue.JObject.static[F](
          src,
          getLabels[m.MirroredElemLabels]
            .zip(values)
            .toMap
        )
