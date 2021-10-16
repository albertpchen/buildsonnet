package root

import scala.compiletime.{constValue, erasedValue, summonInline, summonFrom}
import scala.deriving.Mirror
import scala.concurrent.{ExecutionContext, Future}
import cats.data.Nested
import cats.syntax.all.given
import monix.eval.Task

final class JDecoderPath(
  path: Seq[String | Int]
):
  def isEmpty: Boolean = path.isEmpty
  def error(ctx: EvaluationContext, src: Source, msg: String): EvaluatedJValue.JError =
    if isEmpty then
      ctx.error(src, msg)
    else
      ctx.error(src, s"error at path $toString, $msg")

  inline def expectType[T](ctx: EvaluationContext, expr: EvaluatedJValue): TypedJValue[T] =
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
  def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[T]
  final def decode(ctx: EvaluationContext, expr: EvaluatedJValue): TypedJValueTask[T] = decode(ctx, JDecoderPath.empty, expr)

sealed trait JObjectDecoder[T]:
  def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject): TypedJValueTask[T]
  final def decode(ctx: EvaluationContext, obj: EvaluatedJValue.JObject): TypedJValueTask[T] = decode(ctx, JDecoderPath.empty, obj)

object JDecoder:
  def apply[T](using decoder: JDecoder[T]): JDecoder[T] = decoder

  given JDecoder[String] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[String] =
      given ExecutionContext = ctx.executionContext
      Task.now(path.expectType[String](ctx, expr))

  given JDecoder[Double] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[Double] =
      given ExecutionContext = ctx.executionContext
      Task.now(path.expectType[Double](ctx, expr))

  given JDecoder[Int] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[Int] =
      given ExecutionContext = ctx.executionContext
      Task.now(path.expectType[Double](ctx, expr).map(_.toInt))

  given JDecoder[Boolean] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[Boolean] =
      given ExecutionContext = ctx.executionContext
      Task.now(path.expectType[Boolean](ctx, expr))

  import bloop.config.PlatformFiles.Path
  given JDecoder[Path] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[Path] =
      given ExecutionContext = ctx.executionContext
      Task.now(path.expectType[EvaluatedJValue.JPath | EvaluatedJValue.JString](ctx, expr).map {
        case p: EvaluatedJValue.JPath => p.path
        case s: EvaluatedJValue.JString =>
          if s.str.startsWith("/") then
            java.nio.file.Paths.get(s.str).normalize
          else
            ctx.workspaceDir.resolve(s.str).normalize
      })
/*
  inline given identityDecoder[T <: EvaluatedJValue]: JDecoder[T] = identity[T]

  inline def identity[T <: EvaluatedJValue]: JDecoder[T] = new JDecoder[T]:
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[T] =
      expr match
      case t: T => Task.now(TypedJValue(t))
      case e => Task.now(TypedJValue.error(ctx.error(src, s"expected type ${}")))
*/

  given iterableDecoder[L[X] <: Iterable[X], T](using f: collection.Factory[T, L[T]], d: => JDecoder[T]): JDecoder[L[T]] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[L[T]] =
      given ExecutionContext = ctx.executionContext
      val tasks = path.expectType[Seq[EvaluatedJValue]](ctx, expr).map { elements =>
        elements.foldLeft(Task.now(TypedJValue(Seq.empty[T]))) { (acc, e) =>
          acc.zip(d.decode(ctx, e)).map((acc, e) => e.flatMap(e => acc.map(e +: _)))
        }
      }
      tasks.toEither.fold(
        e => Task.now(TypedJValue.error(e)),
        _.map(_.map(f.fromSpecific)),
      )

  given mapDecoder[T](using => JDecoder[T]): JDecoder[Map[String, T]] with
    def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue): TypedJValueTask[Map[String, T]] =
      given ExecutionContext = ctx.executionContext
      path.expectType[EvaluatedJValue.JObject](ctx, expr).toEither.fold(
        e => Task.now(TypedJValue.error(e)),
        obj => {
          val nested = obj.members().foldLeft(Task.now(TypedJValue(Seq.empty[(String, T)]))) {
            case (acc, (key, lazyValue)) => acc.zip(lazyValue.evaluated).flatMap { (acc, lazyValue) =>
              acc.toEither.fold(
                e => Task.now(TypedJValue.error(e)),
                (acc: Seq[(String, T)]) => {
                  Nested[Task, TypedJValue, T](summon[JDecoder[T]].decode(ctx, lazyValue)).map(v => (key -> v) +: acc).value
                }
              )
            }
          }
          Nested(nested).map(_.toMap).value
        }
      )
  
  transparent inline def decodeProduct[T <: Product]: JObjectDecoder[T] =
    inline erasedValue[T] match
      case _: EmptyTuple =>
        val decoder = new JObjectDecoder[EmptyTuple]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            given ExecutionContext = ctx.executionContext
            Task.now(TypedJValue(EmptyTuple))
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, Option[head]) *: tail) =>
        val decoder = new JObjectDecoder[(Option[head] *: tail)]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            given ExecutionContext = ctx.executionContext
            val field = constValue[name].toString
            val head = obj.imp.lookupOpt(obj.src, field).fold[Nested[Task, TypedJValue, Option[head]]](
              Nested(Task(TypedJValue(None)))
            ) { lvalue =>
              Nested(lvalue.evaluated.flatMap(summonInline[JDecoder[head]].decode(ctx, path.withField(field), _))).map(Some(_))
            }
            val tail = decodeProduct[tail].decode(ctx, path, obj)
            // head.zip(tail).map(_ *: _)
            (head, Nested[Task, TypedJValue, tail](tail)).mapN(_ *: _).value
        decoder.asInstanceOf[JObjectDecoder[T]]
      case _: ((name, head) *: tail) =>
        val decoder = new JObjectDecoder[(head *: tail)]:
          def decode(ctx: EvaluationContext, path: JDecoderPath, obj: EvaluatedJValue.JObject) =
            val field = constValue[name].toString
            val value = obj.lookup(obj.src, field)
            val head = value.flatMap(summonInline[JDecoder[head]].decode(ctx, path.withField(field), _))
            val tail = decodeProduct[tail].decode(ctx, path, obj)
            given ExecutionContext = ctx.executionContext
            (Nested[Task, TypedJValue, head](head), Nested[Task, TypedJValue, tail](tail)).mapN(_ *: _).value
        decoder.asInstanceOf[JObjectDecoder[T]]

  inline given [T: Mirror.ProductOf]: JDecoder[T] = derived[T]

  inline def derived[T](using m: Mirror.ProductOf[T]): JDecoder[T] =
    type Zipped = Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]
    val objDecoder = decodeProduct[Zipped]
    new JDecoder[T]:
      def decode(ctx: EvaluationContext, path: JDecoderPath, expr: EvaluatedJValue) =
        given ExecutionContext = ctx.executionContext
        path.expectType[EvaluatedJValue.JObject](ctx, expr).fold(
          e => Task.now(TypedJValue.error(e)),
          obj => Nested[Task, TypedJValue, Zipped](objDecoder.decode(ctx, path, obj)).map(m.fromProduct).value,
        )

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
