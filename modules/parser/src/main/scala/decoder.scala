package root

import shapeless3.deriving.*
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.concurrent.{ExecutionContext, Future}

final class JDecoderPath(
  path: Seq[String | Int]
):
  def isEmpty: Boolean = path.isEmpty
  inline def expectType[T <: EvaluatedJValue.JNow](ctx: EvaluationContext, expr: EvaluatedJValue): Future[T] =
    if isEmpty then
      ctx.expectType[T](expr)
    else
      ctx.expectType[T](expr, s"error at path $toString, unexpected type ${EvaluationContext.typeString(theExpr)}, expected ${EvaluationContext.typeString[T]}")

  def withField(field: String): JDecoderPath =
    new JDecoderPath(field +: path)

  def withIndex(idx: Int): JDecoderPath =
    new JDecoderPath(idx +: path)

  override def toString: String =
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

object JDecoderPath:
  def empty: JDecoderPath = new JDecoderPath(Seq.empty)

sealed trait JDecoder[T]:
  def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[T]
  final def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[T] = decode(ctx, JDecoderPath.empty, expr)

sealed trait JObjectDecoder[T]:
  def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject): Future[T]
  final def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject): Future[T] = decode(ctx, JDecoderPath.empty, obj)

object JDecoder:
  def apply[T](using decoder: JDecoder[T]): JDecoder[T] = decoder

  given JDecoder[String] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[String] =
      given ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JString](ctx, expr).map(_.str)

  given JDecoder[Double] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[Double] =
      given ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JNum](ctx, expr).map(_.double)

  given JDecoder[Int] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[Int] =
      given ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JNum](ctx, expr).map(_.double.toInt)

  given JDecoder[Boolean] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[Boolean] =
      given ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JBoolean](ctx, expr).map(_.value)

  inline given [T <: EvaluatedJValue.JNow]: JDecoder[T] = identity[T]

  inline def identity[T <: EvaluatedJValue.JNow]: JDecoder[T] = new JDecoder[T]:
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[T] =
      path.expectType[T](ctx, expr)

  given [T: JDecoder]: JDecoder[Seq[T]] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[Seq[T]] =
      given ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JArray](ctx, expr).flatMap { arr =>
        val elements: Seq[Future[T]] = arr.elements.map(summon[JDecoder[T]].decode(ctx, path, _))
        Future.sequence(elements)
      }

  given [T: JDecoder]: JDecoder[Map[String, T]] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Future[Map[String, T]] =
      given ExecutionContext = ctx.executionContext
      val nested = for
        obj <- path.expectType[EvaluatedJValue.JObject](ctx, expr)
        members <- obj.members()
      yield
        Future.sequence(members.map { (key, lazyValue) =>
          summon[JDecoder[T]].decode(ctx, path, lazyValue.evaluated).map(key -> _)
        }).map(_.toMap)
      nested.flatten
  
  transparent inline def decodeProduct[T <: Product]: JObjectDecoder[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        val decoder = new JObjectDecoder[EmptyTuple]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            given ExecutionContext = ctx.executionContext
            Future(EmptyTuple)
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, Option[head]) *: tail) =>
        val decoder = new JObjectDecoder[(Option[head] *: tail)]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            given ExecutionContext = ctx.executionContext
            val head =
              obj.imp.lookupOpt(obj.src, constValue[name].toString).flatMap { lvalueOpt =>
                lvalueOpt.fold(Future(None)) { lvalue =>
                  summonInline[JDecoder[head]].decode(ctx, path, lvalue.evaluated).map(Some(_))
                }
              }
            val tail = decodeProduct[tail].decode(ctx, path, obj)
            head.zip(tail).map(_ *: _)
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, head) *: tail) =>
        val decoder = new JObjectDecoder[(head *: tail)]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            val field = constValue[name].toString
            val value = obj.lookup(obj.src, field)
            val head = summonInline[JDecoder[head]].decode(ctx, path.withField(field), value)
            val tail = decodeProduct[tail].decode(ctx, path, obj)
            given ExecutionContext = ctx.executionContext
            head.zip(tail).map(_ *: _)
        decoder.asInstanceOf[JObjectDecoder[T]]

  inline given [T: Mirror.ProductOf]: JDecoder[T] = derived[T]

  inline def derived[T](using m: Mirror.ProductOf[T]): JDecoder[T] =
    val objDecoder = decodeProduct[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]]
    new JDecoder[T]:
      def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue) =
        given ExecutionContext = ctx.executionContext
        path.expectType[EvaluatedJValue.JObject](ctx, expr).flatMap(objDecoder.decode(ctx, path, _)).map {
          m.fromProduct
        }

import coursier.{Dependency, Fetch, Module, ModuleName, Organization}

case class CoursierParams(
  deps: Seq[CoursierDependency]
) derives JDecoder

case class CoursierDependency(
  org: String,
  name: String,
  version: String,
) derives JDecoder:
  def toDependency: Dependency =
    Dependency(Module(Organization(org), ModuleName(name)), version)
