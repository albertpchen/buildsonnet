package buildsonnet.evaluator

import cats.{Monad, MonadError, Parallel}
import cats.effect.{Async, Sync}
import cats.syntax.all.given

import buildsonnet.ast.*
import buildsonnet.logger.ConsoleLogger

import java.nio.file.Path

trait EvaluationContext[F[_]]:
  def error[T](src: Source, msg: String): F[T]
  def lookup(id: String): Option[LazyValue[F]]

  def bind(id: String, value: LazyValue[F]): EvaluationContext[F]
  def bind(locals: List[(String, LazyValue[F])]): EvaluationContext[F]

  def self: Option[EvaluatedJValue.JObject[F]]
  def `super`: Option[EvaluatedJValue.JObject[F]]

  def withSelf(obj: EvaluatedJValue.JObject[F]): EvaluationContext[F]
  def withSuper(obj: Option[EvaluatedJValue.JObject[F]]): EvaluationContext[F]

  def `import`(src: Source, file: String): F[EvaluatedJValue[F]]
  def importStr(src: Source, file: String): F[EvaluatedJValue.JString[F]]

  def withStackEntry(entry: StackEntry): EvaluationContext[F]

  def workspaceDir: Path
  def file: SourceFile

object EvaluationContext:
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
      case _: EvaluatedJValue.JArray[F] => "array"
      case _: EvaluatedJValue.JObject[F] => "object"
      case _: EvaluatedJValue.JFunction[F] => "function"
    }
    if types.size == 1 then types.head
    else if types.size == 2 then s"${types(0)} or ${types(1)}"
    else
      types.reverse.tail.fold(s"or ${types.last}") { (str, tpe) =>
        s"$tpe, $str"
      }

  def typeString[F[_]](expr: EvaluatedJValue[F]): String =
    expr match
    case _: EvaluatedJValue.JBoolean[F] => "bool"
    case _: EvaluatedJValue.JNull[F] => "null"
    case _: EvaluatedJValue.JString[F] => "string"
    case _: EvaluatedJValue.JNum[F] => "number"
    case _: EvaluatedJValue.JArray[F] => "array"
    case _: EvaluatedJValue.JObject[F] => "object"
    case _: EvaluatedJValue.JFunction[F] => "function"

  extension [F[_]: Monad](ctx: EvaluationContext[F])
    inline def decode[T: [X] =>> JDecoder[F, X]](expr: EvaluatedJValue[F]): F[T] =
      JDecoder[F, T].decode(ctx, expr)

    inline def encode[T: [X] =>> JEncoder[F, X]](src: Source, t: T): EvaluatedJValue[F] =
      JEncoder[F, T].encode(ctx, src, t)

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

    inline def expectFieldName(
      jvalue: EvaluatedJValue[F],
    ): F[EvaluatedJValue.JNull[F] | EvaluatedJValue.JString[F]] =
      expect[EvaluatedJValue.JNull[F] | EvaluatedJValue.JString[F]](
        jvalue,
        s"Field name must be string or null, got ${typeString(theExpr)}",
      )

  extension [F[_]: Async: ConsoleLogger: Parallel](ctx: EvaluationContext[F])
    inline def expect[T](jvalue: JValue): F[T] =
      eval(ctx)(jvalue).flatMap(ctx.expect[T](_))

    def bindStrict(id: String, value: EvaluatedJValue[F]): EvaluationContext[F] =
      ctx.bind(id, LazyValue.strict(value))
    def bindStrict(locals: List[(String, EvaluatedJValue[F])]): EvaluationContext[F] =
      locals.foldLeft(ctx) { case (ctx, (key, value)) =>
        ctx.bindStrict(key, value)
      }

    def bindCode(id: String, value: JValue): F[EvaluationContext[F]] =
      LazyValue(eval(ctx)(value)).map(ctx.bind(id, _))
    def bindCode(locals: List[(String, JValue)]): F[EvaluationContext[F]] =
      locals.foldLeft(ctx.pure) { case (ctx, (key, value)) =>
        ctx.flatMap(_.bindCode(key, value))
      }

    def prettyPrint(value: EvaluatedJValue[F]): F[String] = Sync[F].defer {
      val builder = new StringBuilder
      prettyPrintImp("   ", 0, None, builder, value.src, value) *> Sync[F].delay(builder.toString)
    }

    def singleLinePrint(value: EvaluatedJValue[F]): F[String] = Sync[F].defer {
      val builder = new StringBuilder
      singleLinePrintImp(builder, value.src, value) *> Sync[F].delay(builder.toString)
    }

    private def singleLinePrintImp(
      builder: StringBuilder,
      src: Source,
      value: EvaluatedJValue[F],
    ): F[Unit] = Sync[F].defer {
      import EvaluatedJValue.*
      value match
      case _: JFunction[F] => ctx.error(src, "couldn't manifest function in JSON output")
      case JNull(_) => builder ++= "null"; ().pure
      case JString(_, string) =>
        builder += '"'
        EvaluatedJValue.escape(string, builder)
        builder += '"'
        ().pure
      case JNum(_, value) =>
        if value.isWhole then builder ++= value.toLong.toString
        else builder ++= value.toString
        ().pure
      case JBoolean(_, value) => builder ++= (if value then "true" else "false"); ().pure
      case JArray(_, value) if value.isEmpty => builder ++= "[]"; ().pure
      case JArray(_, value) =>
        Sync[F].defer {
          builder += '['
          singleLinePrintImp(builder, src, value.head) *>
            value.tail.foldLeft(().pure) { (prev, e) =>
              prev *> Sync[F].delay(builder ++= ", ") *> singleLinePrintImp(builder, src, e)
            } *>
            Sync[F].delay(builder += ']')
        }
      case obj: JObject[F] if obj.members.isEmpty => builder ++= "{ }"; ().pure
      case obj: JObject[F] =>
        val value = obj.members.toSeq.sortBy(_._1)
        builder ++= "{ \""
        EvaluatedJValue.escape(value.head._1, builder)
        builder ++= "\": "
        value.head._2.value.flatMap(singleLinePrintImp(builder, src, _)) *>
          value.tail.foldLeft(().pure[F]) { case (prev, (k, v)) =>
            prev *> Sync[F].defer {
              builder ++= ", \""
              EvaluatedJValue.escape(k, builder)
              builder ++= "\": "
              v.value.flatMap(singleLinePrintImp(builder, src, _))
            }
          } *>
          Sync[F].delay {
            builder += '}'
          }
    }

    private def prettyPrintImp(
      tab: String,
      tabNum: Int,
      firstPrefix: Option[String],
      builder: StringBuilder,
      src: Source,
      value: EvaluatedJValue[F],
    ): F[Unit] = Sync[F].defer {
      import EvaluatedJValue.*
      val prefix = tab * tabNum
      builder ++= firstPrefix.getOrElse(prefix)
      value match
      case _: JFunction[F] => ctx.error(src, "couldn't manifest function in JSON output")
      case JNull(_) => builder ++= "null"; ().pure
      case JString(_, string) =>
        builder += '"'
        EvaluatedJValue.escape(string, builder)
        builder += '"'
        ().pure
      case JNum(_, value) =>
        if value.isWhole then builder ++= value.toLong.toString
        else builder ++= value.toString
        ().pure
      case JBoolean(_, value) => builder ++= (if value then "true" else "false"); ().pure
      case JArray(_, value) if value.isEmpty => builder ++= "[]"; ().pure
      case JArray(_, value) =>
        builder ++= "[\n"
        prettyPrintImp(tab, tabNum + 1, None, builder, src, value.head) *>
          value.tail.foldLeft(().pure[F]) { (sync, e) =>
            sync *> Sync[F].defer {
              builder ++= ",\n"
              prettyPrintImp(tab, tabNum + 1, None, builder, src, e)
            }
          } *> Sync[F].delay {
            builder += '\n'
            builder ++= prefix
            builder += ']'
          }
      case obj: JObject[?] if obj.members.isEmpty => builder ++= "{ }"; ().pure
      case obj: JObject[?] =>
        val value = obj.asInstanceOf[JObject[F]].members.toSeq.sortBy(_._1)
        builder ++= "{\n"
        builder ++= prefix
        builder ++= tab
        builder += '"'
        EvaluatedJValue.escape(value.head._1, builder)
        value
          .head
          ._2
          .value
          .flatMap(prettyPrintImp(tab, tabNum + 1, Some("\": "), builder, src, _)) *>
          value.tail.foldLeft(().pure[F]) { case (sync, (k, v)) =>
            sync *> Sync[F].defer {
              builder ++= ",\n"
              builder ++= prefix
              builder ++= tab
              builder += '"'
              EvaluatedJValue.escape(k, builder)
              v.value.flatMap(prettyPrintImp(tab, tabNum + 1, Some("\": "), builder, src, _))
            }
          } *>
          Sync[F].delay {
            builder += '\n'
            builder ++= prefix
            builder += '}'
          }
    }
