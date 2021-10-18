package root

import monix.eval.Task

import scala.compiletime.{constValue, erasedValue, summonInline, summonFrom}
import scala.deriving.Mirror

final class JDecoderPath(
  path: Seq[String | Int]
):
  def isEmpty: Boolean = path.isEmpty
  def error[T <: EvaluatedJValue.JNow](ctx: EvaluationContext, src: Source, msg: String): Nothing =
    if isEmpty then
      ctx.error(src, msg)
    else
      ctx.error(src, s"error at path $toString, $msg")

  inline def expectType[T <: EvaluatedJValue.JNow](ctx: EvaluationContext, expr: EvaluatedJValue): Task[T] =
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

trait JDecoder[T]:
  def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[T]
  final def decode(ctx: EvaluationContext, expr: EvaluatedJValue): Task[T] = decode(ctx, JDecoderPath.empty, expr)

sealed trait JObjectDecoder[T]:
  def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject): Task[T]
  final def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject): Task[T] = decode(ctx, JDecoderPath.empty, obj)

object JDecoder:
  def apply[T](using decoder: JDecoder[T]): JDecoder[T] = decoder

  given JDecoder[String] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[String] =
      path.expectType[EvaluatedJValue.JString](ctx, expr).map(_.str)

  given JDecoder[Double] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[Double] =
      path.expectType[EvaluatedJValue.JNum](ctx, expr).map(_.double)

  given JDecoder[Int] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[Int] =
      path.expectType[EvaluatedJValue.JNum](ctx, expr).map(_.double.toInt)

  given JDecoder[Boolean] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[Boolean] =
      path.expectType[EvaluatedJValue.JBoolean](ctx, expr).map(_.value)

  import bloop.config.PlatformFiles.Path
  given JDecoder[Path] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[Path] =
      path.expectType[EvaluatedJValue.JPath | EvaluatedJValue.JString](ctx, expr).map {
        case p: EvaluatedJValue.JPath => p.path
        case s: EvaluatedJValue.JString =>
          if s.str.startsWith("/") then
            java.nio.file.Paths.get(s.str).normalize
          else
            ctx.workspaceDir.resolve(s.str).normalize
      }

  inline given identityDecoder[T <: EvaluatedJValue.JNow]: JDecoder[T] = identity[T]

  inline def identity[T <: EvaluatedJValue.JNow]: JDecoder[T] = new JDecoder[T]:
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[T] =
      path.expectType[T](ctx, expr)

  given iterableDecoder[L[X] <: Iterable[X], T](using f: collection.Factory[T, L[T]], d: => JDecoder[T]): JDecoder[L[T]] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[L[T]] =
      path.expectType[EvaluatedJValue.JArray](ctx, expr).flatMap { arr =>
        val elements: Seq[Task[T]] = arr.elements.map(summon[JDecoder[T]].decode(ctx, path, _))
        Task.sequence(elements).map(f.fromSpecific)
      }

  given mapDecoder[T](using => JDecoder[T]): JDecoder[Map[String, T]] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): Task[Map[String, T]] =
      val nested = for
        obj <- path.expectType[EvaluatedJValue.JObject](ctx, expr)
      yield
        Task.sequence(obj.members().map { (key, lazyValue) =>
          summon[JDecoder[T]].decode(ctx, path, lazyValue.evaluated).map(key -> _)
        }).map(_.toMap)
      nested.flatten
  
  transparent inline def decodeProduct[T <: Product]: JObjectDecoder[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        val decoder = new JObjectDecoder[EmptyTuple]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            Task.now(EmptyTuple)
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, Option[head]) *: tail) =>
        val decoder = new JObjectDecoder[(Option[head] *: tail)]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            val field = constValue[name].toString
            val head = obj.imp.lookupOpt(obj.src, field).fold(Task.now(None)) { lvalue =>
              summonInline[JDecoder[head]].decode(ctx, path.withField(field), lvalue.evaluated).map(Some(_))
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
            head.zip(tail).map(_ *: _)
        decoder.asInstanceOf[JObjectDecoder[T]]

  inline given [T: Mirror.ProductOf]: JDecoder[T] = derived[T]

  inline def derived[T](using m: Mirror.ProductOf[T]): JDecoder[T] =
    val objDecoder = decodeProduct[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]]
    new JDecoder[T]:
      def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue) =
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
