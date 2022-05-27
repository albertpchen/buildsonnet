package buildsonnet

import cats.effect.Sync
import cats.syntax.all.given

import buildsonnet.ast.{JParamList, Source, SourceFile}
import buildsonnet.evaluator.{EvaluationContext, EvaluatedJValue, LazyValue, eval}

import scala.language.dynamics

final class Std[F[_]: Sync] private ():
  import Std.*

  given ctx[F[_]](using a: EvaluationContext[F]): EvaluationContext[F] = a
  given src[F[_]](using a: Source): Source = a

  opaque type Arg[Name] = Option[EvaluatedJValue[F]]

  object Arg extends Dynamic:
    def selectDynamic(name: String): Arg[name.type] = None
    def applyDynamic(name: String)(default: EvaluatedJValue[F]): Arg[name.type] = Some(default)

  extension [T](arg: Arg[T])
    def default: Option[EvaluatedJValue[F]] = arg

  private def bindArgs(
    params: EvaluatedJValue.JFunctionParameters[F],
    paramsDef: Array[(String, Option[EvaluatedJValue[F]])],
  ): F[Array[EvaluatedJValue[F]]] = {
    val positionalArgs = params.positionalArgs
    val namedArgs = params.namedArgs
    val numGivenArgs = positionalArgs.size + namedArgs.size
    for
      _ <- if numGivenArgs > paramsDef.size then
        params.ctx.error(params.src, "to many arguments for function")
      else
        ().pure[F]
      argMap = namedArgs.toMap
      result <- {
        var currPosArgs = positionalArgs
        var error = Option.empty[F[Nothing]]
        var i = 0
        var locals = Array.ofDim[EvaluatedJValue[F]](paramsDef.size)
        while error.isEmpty && i < paramsDef.size do
          val (argName, default) = paramsDef(i)
          val isGivenNamedArg = argMap.contains(argName)
          if positionalArgs.nonEmpty && isGivenNamedArg then
            error = Some(params.ctx.error(params.src, s"both positional and named arg provided for argument $argName"))
          else if currPosArgs.nonEmpty then
            locals(i) = currPosArgs.head
            currPosArgs = currPosArgs.tail
          else if isGivenNamedArg then
            locals(i) = argMap(argName)
          else if default.isDefined then
            locals(i) = default.get
          else
            error = Some(params.ctx.error(params.src, s"missing argument $argName"))
          i += 1
        error.fold(locals.pure)(e => e.widen)
      }
    yield
      result
  }

  private inline def function1[Name1 <: String](
    arg1: Arg[Name1],
  )(
    fn: (EvaluationContext[F], Source) ?=> EvaluatedJValue[F] => F[EvaluatedJValue[F]],
  ): EvaluatedJValue.JFunction[F] =
    EvaluatedJValue.JFunction(stdSrc, 1, params => {
      bindArgs(params, Array(
        compiletime.constValue[Name1] -> arg1.default
      )).flatMap { case Array(a1) =>
        fn(using params.ctx, params.src)(a1)
      }
    })

  val members: Map[String, EvaluatedJValue[F]] = Map(
    "type" -> function1(Arg.x) { (x) =>
      EvaluatedJValue.JString(src, EvaluationContext.typeString(x)).pure
    },
  )

object Std:
  private val stdSrc = Source.Generated(SourceFile.std)

  def apply[F[_]: Sync](ctx: EvaluationContext[F]): EvaluatedJValue.JObject[F] =
    EvaluatedJValue.JObject.static(
      stdSrc,
      ctx,
      new Std[F]().members
    )
