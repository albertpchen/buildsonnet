package buildsonnet.evaluator

import cats.{Monad, Parallel}
import cats.syntax.all.given

import buildsonnet.ast.{Source}

import scala.compiletime.{constValue, erasedValue, summonInline, summonFrom}
import scala.deriving.Mirror

opaque type JDecoderPath = Seq[String | Int]

object JDecoderPath:
  extension (path: JDecoderPath)
    def isEmpty: Boolean = path.isEmpty

    def error[F[_], T <: EvaluatedJValue[F], A](
      ctx: EvaluationContext[F],
      src: Source,
      msg: String,
    ): F[A] =
      if isEmpty then ctx.error(src, msg)
      else ctx.error(src, s"error at path $toString, $msg")

    inline def expect[F[_]: Monad, T](ctx: EvaluationContext[F], expr: EvaluatedJValue[F]): F[T] =
      if isEmpty then ctx.expect[T](expr)
      else
        ctx.expect[T](
          expr,
          s"error at path $toString, unexpected type ${EvaluationContext
              .typeString(EvaluationContext.theExpr)}, expected ${EvaluationContext.typeString[F, T]}",
        )

    def withField(field: String): JDecoderPath =
      field +: path

    def withIndex(idx: Int): JDecoderPath =
      idx +: path

    def toString: String =
      val builder = new StringBuilder()
      path.reverse.foreach {
        case field: String =>
          builder += '.'
          builder ++= field
        case idx: Int =>
          builder += '['
          builder ++= idx.toString
          builder += ']'
      }
      builder.toString

  def empty: JDecoderPath = Seq.empty

trait JDecoder[F[_]: Monad, T]:
  def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue[F]): F[T]
  final def decode(ctx: EvaluationContext[F], expr: EvaluatedJValue[F]): F[T] =
    decode(ctx, JDecoderPath.empty, expr)

sealed trait JObjectDecoder[F[_]: Monad, T]:
  def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue.JObject[F]): F[T]
  final def decode(ctx: EvaluationContext[F], expr: EvaluatedJValue.JObject[F]): F[T] =
    decode(ctx, JDecoderPath.empty, expr)

object JDecoder:
  def apply[F[_], T](using decoder: JDecoder[F, T]): JDecoder[F, T] = decoder

  given [F[_]: Monad]: JDecoder[F, String] with
    def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue[F]): F[String] =
      path.expect[F, String](ctx, expr)

  given [F[_]: Monad]: JDecoder[F, Double] with
    def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue[F]): F[Double] =
      path.expect[F, Double](ctx, expr)

  given [F[_]: Monad]: JDecoder[F, Int] with
    def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue[F]): F[Int] =
      path.expect[F, Double](ctx, expr).map(_.toInt)

  given [F[_]: Monad]: JDecoder[F, Boolean] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[Boolean] =
      path.expect[F, Boolean](ctx, expr)

  given [F[_]: Monad]: JDecoder[F, java.nio.file.Path] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[java.nio.file.Path] =
      path.expect[F, String](ctx, expr).map { str =>
        if str.startsWith("/") then java.nio.file.Paths.get(str).normalize
        else ctx.workspaceDir.resolve(str).normalize
      }

  given iterableDecoder[F[_]: Monad: Parallel, L[X] <: Iterable[X], T](using
    f: collection.Factory[T, L[T]],
    d: => JDecoder[F, T],
  ): JDecoder[F, L[T]] with
    def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue[F]): F[L[T]] =
      path.expect[F, EvaluatedJValue.JArray[F]](ctx, expr).flatMap { arr =>
        val elements: Seq[F[T]] = arr.elements.map(summon[JDecoder[F, T]].decode(ctx, path, _))
        elements.sequence.map(f.fromSpecific)
      }

  given mapDecoder[F[_]: Monad: Parallel, T](using => JDecoder[F, T]): JDecoder[F, Map[String, T]]
    with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[Map[String, T]] =
      for
        obj <- path.expect[F, EvaluatedJValue.JObject[F]](ctx, expr)
        decoded <- obj.members.toSeq.traverse { (key, lazyValue) =>
          lazyValue.value.flatMap { value =>
            summon[JDecoder[F, T]].decode(ctx, path, value).map(key -> _)
          }
        }
      yield decoded.toMap

  given optionDecoder[F[_]: Monad, T](using => JDecoder[F, T]): JDecoder[F, Option[T]] with
    def decode(
      ctx: EvaluationContext[F],
      path: JDecoderPath,
      expr: EvaluatedJValue[F],
    ): F[Option[T]] =
      expr match
      case _: EvaluatedJValue.JNull[F] => None.pure
      case other => JDecoder[F, T].decode(ctx, path, other).map(Some(_))

  transparent inline def decodeProduct[F[_]: Monad, T <: Product]: JObjectDecoder[F, T] =
    inline erasedValue[T] match
    case _: EmptyTuple =>
      val decoder = new JObjectDecoder[F, EmptyTuple]:
        def decode(ctx: EvaluationContext[F], path: JDecoderPath, obj: EvaluatedJValue.JObject[F]) =
          EmptyTuple.pure
      decoder.asInstanceOf[JObjectDecoder[F, T]]
    case _: ((name, Option[head]) *: tail) =>
      val decoder = new JObjectDecoder[F, (Option[head] *: tail)]:
        def decode(ctx: EvaluationContext[F], path: JDecoderPath, obj: EvaluatedJValue.JObject[F]) =
          val field = constValue[name].toString
          val head = obj.lookupOpt(field).fold(None.pure) { lvalue =>
            for
              value <- lvalue.value
              decoded <- summonInline[JDecoder[F, head]].decode(ctx, path.withField(field), value)
            yield Some(decoded)
          }
          val tail = decodeProduct[F, tail].decode(ctx, path, obj)
          (head, tail).mapN(_ *: _)
      decoder.asInstanceOf[JObjectDecoder[F, T]]
    case _: ((name, head) *: tail) =>
      val decoder = new JObjectDecoder[F, (head *: tail)]:
        def decode(ctx: EvaluationContext[F], path: JDecoderPath, obj: EvaluatedJValue.JObject[F]) =
          val field = constValue[name].toString
          for
            value <- obj
              .lookupOpt(field)
              .fold(ctx.error(obj.src, s"object does not have field $field"))(_.value)
            head <- summonInline[JDecoder[F, head]].decode(ctx, path.withField(field), value)
            tail <- decodeProduct[F, tail].decode(ctx, path, obj)
          yield head *: tail
      decoder.asInstanceOf[JObjectDecoder[F, T]]

  inline given [F[_]: Monad, T: Mirror.ProductOf]: JDecoder[F, T] = derived[F, T]

  inline def derived[F[_]: Monad, T](using m: Mirror.ProductOf[T]): JDecoder[F, T] =
    val objDecoder = decodeProduct[F, Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]]
    new JDecoder[F, T]:
      def decode(ctx: EvaluationContext[F], path: JDecoderPath, expr: EvaluatedJValue[F]) =
        path
          .expect[F, EvaluatedJValue.JObject[F]](ctx, expr)
          .flatMap(objDecoder.decode(ctx, path, _))
          .map(m.fromProduct)
