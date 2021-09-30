package root

import shapeless3.deriving.*
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.concurrent.{ExecutionContext, Future}

sealed trait JDecoder[T]:
  def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[T]

sealed trait JObjectDecoder[T]:
  def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject): Future[T]

object JDecoder:
  def apply[T](using decoder: JDecoder[T]): JDecoder[T] = decoder

  given JDecoder[String] with
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[String] =
      given ExecutionContext = ctx.executionContext
      ctx.expectString(expr).map(_.str)

  given JDecoder[Double] with
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[Double] =
      given ExecutionContext = ctx.executionContext
      ctx.expectNum(expr).map(_.double)

  given JDecoder[Int] with
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[Int] =
      given ExecutionContext = ctx.executionContext
      ctx.expectNum(expr).map(_.double.toInt)

  given JDecoder[Boolean] with
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[Boolean] =
      given ExecutionContext = ctx.executionContext
      ctx.expectBoolean(expr).map(_.value)

  inline given [T <: EvaluatedJValue.JNow]: JDecoder[T] = identity[T]

  inline def identity[T <: EvaluatedJValue.JNow]: JDecoder[T] = new JDecoder[T]:
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[T] =
      ctx.expectType[T](expr)

  given [T: JDecoder]: JDecoder[Seq[T]] with
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[Seq[T]] =
      given ExecutionContext = ctx.executionContext
      ctx.expectArray(expr).flatMap { arr =>
        val elements: Seq[Future[T]] = arr.elements.map(summon[JDecoder[T]].decode(ctx, _))
        Future.sequence(elements)
      }

  given [T: JDecoder]: JDecoder[Map[String, T]] with
    def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Future[Map[String, T]] =
      given ExecutionContext = ctx.executionContext
      val nested = for
        obj <- ctx.expectObject(expr)
        members <- obj.members()
      yield
        Future.sequence(members.map { (key, lazyValue) =>
          summon[JDecoder[T]].decode(ctx, lazyValue.evaluated).map(key -> _)
        }).map(_.toMap)
      nested.flatten
  
  transparent inline def decodeProduct[T <: Product]: JObjectDecoder[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        val decoder = new JObjectDecoder[EmptyTuple]:
          def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject) =
            given ExecutionContext = ctx.executionContext
            Future(EmptyTuple)
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, Option[head]) *: tail) =>
        val decoder = new JObjectDecoder[(Option[head] *: tail)]:
          def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject) =
            given ExecutionContext = ctx.executionContext
            val head =
              obj.imp.lookupOpt(obj.src, constValue[name].toString).flatMap { lvalueOpt =>
                lvalueOpt.fold(Future(None)) { lvalue =>
                  summonInline[JDecoder[head]].decode(ctx, lvalue.evaluated).map(Some(_))
                }
              }
            val tail = decodeProduct[tail].decode(ctx, obj)
            head.zip(tail).map(_ *: _)
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, head) *: tail) =>
        val decoder = new JObjectDecoder[(head *: tail)]:
          def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject) =
            val value = obj.lookup(obj.src, constValue[name].toString)
            val head = summonInline[JDecoder[head]].decode(ctx, value)
            val tail = decodeProduct[tail].decode(ctx, obj)
            given ExecutionContext = ctx.executionContext
            head.zip(tail).map(_ *: _)
        decoder.asInstanceOf[JObjectDecoder[T]]

  inline given [T: Mirror.ProductOf]: JDecoder[T] = derived[T]

  inline def derived[T](using m: Mirror.ProductOf[T]): JDecoder[T] =
    val objDecoder = decodeProduct[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]]
    new JDecoder[T]:
      def decode(ctx: EvaluationContext, expr: EvaluatedJValue) =
        given ExecutionContext = ctx.executionContext
        ctx.expectObject(expr).flatMap(objDecoder.decode(ctx, _)).map {
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
