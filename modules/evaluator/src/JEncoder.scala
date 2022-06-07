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
    def encode(ctx: EvaluationContext[F], src: Source, t: String) =
      EvaluatedJValue.JString[F](src, t)

  given [F[_]]: JEncoder[F, Boolean] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Boolean) =
      EvaluatedJValue.JBoolean[F](src, t)

  given [F[_]]: JEncoder[F, Double] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Double) = EvaluatedJValue.JNum[F](src, t)

  given [F[_]]: JEncoder[F, Int] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Int) = EvaluatedJValue.JNum[F](src, t)

  given [F[_]]: JEncoder[F, Path] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Path) =
      if t.getRoot == null then EvaluatedJValue.JString[F](src, t.toString)
      else EvaluatedJValue.JString[F](src, ctx.workspaceDir.relativize(t).toString)

  given [F[_], L[X] <: Iterable[X], T: [T] =>> JEncoder[F, T]]: JEncoder[F, L[T]] with
    def encode(ctx: EvaluationContext[F], src: Source, t: L[T]) =
      EvaluatedJValue
        .JArray[F](src, IArray.unsafeFromArray(t.map(JEncoder[F, T].encode(ctx, src, _)).toArray))

  given [F[_]: Sync, T: [T] =>> JEncoder[F, T]]: JEncoder[F, Map[String, T]] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Map[String, T]) =
      val members = t.map { (k, v) =>
        k -> JEncoder[F, T].encode(ctx, src, v)
      }
      EvaluatedJValue.JObject.static(src, members)
  /*
  given [F[_], T](using => JEncoder[F, T]): JEncoder[F, Option[T]] with
    def encode(ctx: EvaluationContext[F], src: Source, t: Option[T]): EvaluatedJValue[F] =
      t match
      case None => EvaluatedJValue.JNull[F](src)
      case Some(some) => JEncoder[F, T].encode(ctx, src, some)
   */
  inline given [F[_]: Sync, T: Mirror.ProductOf]: JEncoder[F, T] = derived[F, T]

  inline def summonAll[F[_], T <: Tuple]: List[JEncoder[F, ?]] =
    inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[JEncoder[F, t]] :: summonAll[F, ts]

  inline def getLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => constValue[t].toString :: getLabels[ts]

  transparent inline def encodeProduct[F[_], T <: Product](
    ctx: EvaluationContext[F],
    src: Source,
    elements: Product,
    idx: Int,
  ): Map[String, EvaluatedJValue[F]] =
    inline erasedValue[T] match
    case _: EmptyTuple => Map.empty
    case _: ((name, Option[head]) *: tail) =>
      val field = constValue[name].toString
      val head = elements.productElement(idx) match
      case None => EvaluatedJValue.JNull[F](src)
      case Some(some) => summonInline[JEncoder[F, head]].encode(ctx, src, some.asInstanceOf[head])
      encodeProduct[F, tail](ctx, src, elements, idx + 1) + (field -> head)
    case _: ((name, head) *: tail) =>
      val field = constValue[name].toString
      val head = summonInline[JEncoder[F, head]].encode(
        ctx,
        src,
        elements.productElement(idx).asInstanceOf[head],
      )
      encodeProduct[F, tail](ctx, src, elements, idx + 1) + (field -> head)

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  inline def derived[F[_]: Sync, T](using m: Mirror.ProductOf[T]): JEncoder[F, T] =
    new JEncoder[F, T]:
      def encode(ctx: EvaluationContext[F], src: Source, t: T) =
        val members = encodeProduct[F, Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]](
          ctx,
          src,
          t.asInstanceOf[Product],
          0,
        )
        EvaluatedJValue.JObject.static[F](src, members)
