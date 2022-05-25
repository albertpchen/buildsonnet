package buildsonnet.evaluator

import cats.{Monad, MonadError, Parallel}
import cats.effect.Async
import cats.syntax.all.given

import buildsonnet.ast.*
import buildsonnet.logger.ConsoleLogger

sealed trait EvaluationContext[F[_]]:
  def error[T](src: Source, msg: String): F[T]
  def lookup(id: String): Option[LazyValue[F]]

  def bind(id: String, value: LazyValue[F]): EvaluationContext[F]
  def bind(locals: List[(String, LazyValue[F])]): EvaluationContext[F]

  def self: Option[EvaluatedJValue.JObject[F]]
  def `super`: Option[EvaluatedJValue.JObject[F]]

  def withSelf(obj: EvaluatedJValue.JObject[F]): EvaluationContext[F]
  def withSuper(obj: EvaluatedJValue.JObject[F]): EvaluationContext[F]

  def `import`(src: Source, file: String): F[EvaluatedJValue[F]]
  def importStr(src: Source, file: String): F[EvaluatedJValue.JString[F]]

  def workspaceDir: java.nio.file.Path = ???

object EvaluationContext:
  private case class Impl[F[_]](
    self: Option[EvaluatedJValue.JObject[F]],
    `super`: Option[EvaluatedJValue.JObject[F]],
    scopeArr: Array[(String, LazyValue[F])],
  )(using MonadError[F, Throwable]) extends EvaluationContext[F]:
    lazy val scope: Map[String, LazyValue[F]] = scopeArr.toMap
    def error[T](src: Source, msg: String): F[T] =
      EvaluationError(
        SourceFile.empty, src, msg, List.empty).raiseError

    def lookup(id: String): Option[LazyValue[F]] =
      scope.get(id)
    
    def bind(id: String, value: LazyValue[F]): EvaluationContext[F] =
      this.copy(scopeArr = scopeArr :+ (id -> value))

    def bind(locals: List[(String, LazyValue[F])]): EvaluationContext[F] =
      this.copy(scopeArr = scopeArr :++ locals)

    def withSelf(obj: EvaluatedJValue.JObject[F]): EvaluationContext[F] =
      this.copy(self = Some(obj))

    def withSuper(obj: EvaluatedJValue.JObject[F]): EvaluationContext[F] =
      this.copy(`super` = Some(obj))

    def `import`(src: Source, file: String): F[EvaluatedJValue[F]] = ???
    def importStr(src: Source, file: String): F[EvaluatedJValue.JString[F]] = ???

  def std[F[_]](using MonadError[F, Throwable]): EvaluationContext[F] =
    Impl(None, None, Array.empty)

  given theExpr[F[_]](using expr: EvaluatedJValue[F]): EvaluatedJValue[F] = expr

  inline def typeString[F[_], T]: String =
    val types = macros.mapUnionType[T, String] {
      case _: EvaluatedJValue.JBoolean[F] => "bool"
      case _: Boolean => "bool"
      case _: EvaluatedJValue.JNull[F] => "null"
      case _: EvaluatedJValue.JString[F] => "string"
      case _: String => "string"
      case _: EvaluatedJValue.JNum[F] => "number"
      case _: Double => "double"
      //case _: EvaluatedJValue.JJob[F] => "job"
      //case _: EvaluatedJValue.JPath[F] => "path"
      case _: EvaluatedJValue.JArray[F] => "array"
      case _: EvaluatedJValue.JObject[F] => "object"
      case _: EvaluatedJValue.JFunction[F] => "function"
      //case _: EvaluatedJValue.JFuture[F] => "future"
    }
    if types.size == 1 then
      types.head
    else if types.size == 2 then
      s"${types(0)} or ${types(1)}"
    else
      types.reverse.tail.fold(s"or ${types.last}") {
        (str, tpe) => s"$tpe, $str"
      }

  def typeString[F[_]](expr: EvaluatedJValue[F]): String =
    expr match
    case _: EvaluatedJValue.JBoolean[F] => "bool"
    case _: EvaluatedJValue.JNull[F] => "null"
    case _: EvaluatedJValue.JString[F] => "string"
    case _: EvaluatedJValue.JNum[F] => "number"
    //case _: EvaluatedJValue.JJob[F] => "job"
    //case _: EvaluatedJValue.JPath[F] => "path"
    case _: EvaluatedJValue.JArray[F] => "array"
    case _: EvaluatedJValue.JObject[F] => "object"
    case _: EvaluatedJValue.JFunction[F] => "function"
    //case _: EvaluatedJValue.JFuture[F] => "future"

  extension [F[_]: Monad](ctx: EvaluationContext[F])
    inline def expect[T](jvalue: EvaluatedJValue[F], msg: EvaluatedJValue[F] ?=> String): F[T] =
      inline compiletime.erasedValue[T] match
      case _: Boolean =>
        jvalue match
        case jvalue: EvaluatedJValue.JBoolean[F] => jvalue.bool.asInstanceOf[T].pure
        case jvalue => ctx.error(jvalue.src, msg(using jvalue))
      case _: String =>
        jvalue match
        case jvalue: EvaluatedJValue.JString[F] => jvalue.string.asInstanceOf[T].pure
        case jvalue => ctx.error(jvalue.src, msg(using jvalue))
      case _: Double =>
        jvalue match
        case jvalue: EvaluatedJValue.JNum[F] => jvalue.double.asInstanceOf[T].pure
        case jvalue => ctx.error(jvalue.src, msg(using jvalue))
      case _: EvaluatedJValue[F] =>
        jvalue match
        case jvalue: T => jvalue.pure
        case jvalue => ctx.error(jvalue.src, msg(using jvalue))

    inline def expect[T](jvalue: EvaluatedJValue[F]): F[T] =
      expect[T](jvalue, s"Unexpected type ${typeString(theExpr)}, expected ${typeString[F, T]}")

    inline def expectBoolean(jvalue: EvaluatedJValue[F]): F[Boolean] =
      expect[EvaluatedJValue.JBoolean[F]](jvalue).map(_.bool)

    inline def expectString(jvalue: EvaluatedJValue[F]): F[String] =
      expect[EvaluatedJValue.JString[F]](jvalue).map(_.string)

    inline def expectFieldName(jvalue: EvaluatedJValue[F]): F[EvaluatedJValue.JNull[F] | EvaluatedJValue.JString[F]] =
      expect[EvaluatedJValue.JNull[F] | EvaluatedJValue.JString[F]](
        jvalue, s"Field name must be string or null, got ${typeString(theExpr)}")

  extension [F[_]: Async: ConsoleLogger: Parallel](ctx: EvaluationContext[F])
    inline def expect[T](jvalue: JValue): F[T] =
      eval(ctx)(jvalue).flatMap(ctx.expect[T](_))

    def bindStrict(id: String, value: EvaluatedJValue[F]): EvaluationContext[F] =
      ctx.bind(id, LazyValue.strict(value))
    def bindStrict(locals: List[(String, EvaluatedJValue[F])]): EvaluationContext[F] =
      ctx.bind(locals.map((key, value) => key -> LazyValue.strict(value)))

    def bindCode(id: String, value: JValue): F[EvaluationContext[F]] =
      LazyValue(eval(ctx)(value)).map(ctx.bind(id, _))
    def bindCode(locals: List[(String, JValue)]): F[EvaluationContext[F]] =
      locals
        .map((key, value) => LazyValue(eval(ctx)(value)).map(key -> _))
        .sequence
        .map(ctx.bind(_))
