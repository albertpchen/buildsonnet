package buildsonnet

import cats.{Parallel, Traverse}
import cats.effect.{Async, Sync, Resource}
import cats.syntax.all.given

import buildsonnet.ast.{JParamList, Source, SourceFile}
import buildsonnet.evaluator.{EvaluationContext, EvaluatedJValue, LazyValue, eval}
import buildsonnet.evaluator.given Traverse[Iterable]
import buildsonnet.logger.ConsoleLogger
import buildsonnet.job.{JobCache, JobDescription, SQLiteJobCache, JobRunner}

import org.typelevel.log4cats.Logger

import scala.language.dynamics

private class Std[F[_]: Async: ConsoleLogger: Logger: Parallel] private (
  jobCache: JobCache[F],
):
  import Std.*

  inline given ctx[F[_]](using a: EvaluationContext[F]): EvaluationContext[F] = a
  inline given src[F[_]](using a: Source): Source = a

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

  private inline def function1[
    Name1 <: String,
  ](
    arg1: Arg[Name1],
  )(
    fn: (EvaluationContext[F], Source) ?=> (
      EvaluatedJValue[F],
    ) => F[EvaluatedJValue[F]],
  ): EvaluatedJValue.JFunction[F] =
    EvaluatedJValue.JFunction(stdSrc, 1, params => {
      bindArgs(params, Array(
        compiletime.constValue[Name1] -> arg1.default,
      )).flatMap { case Array(a1) =>
        fn(using params.ctx, params.src)(a1)
      }
    })

  private inline def function2[
    Name1 <: String,
    Name2 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
  )(
    fn: (EvaluationContext[F], Source) ?=> (
      EvaluatedJValue[F],
      EvaluatedJValue[F],
    ) => F[EvaluatedJValue[F]],
  ): EvaluatedJValue.JFunction[F] =
    EvaluatedJValue.JFunction(stdSrc, 2, params => {
      bindArgs(params, Array(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
      )).flatMap { case Array(a1, a2) =>
        fn(using params.ctx, params.src)(a1, a2)
      }
    })

  private inline def function3[
    Name1 <: String,
    Name2 <: String,
    Name3 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
    arg3: Arg[Name3],
  )(
    fn: (EvaluationContext[F], Source) ?=> (
      EvaluatedJValue[F],
      EvaluatedJValue[F],
      EvaluatedJValue[F],
    ) => F[EvaluatedJValue[F]],
  ): EvaluatedJValue.JFunction[F] =
    EvaluatedJValue.JFunction(stdSrc, 3, params => {
      bindArgs(params, Array(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
      )).flatMap { case Array(a1, a2, a3) =>
        fn(using params.ctx, params.src)(a1, a2, a3)
      }
    })

  private inline def function4[
    Name1 <: String,
    Name2 <: String,
    Name3 <: String,
    Name4 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
    arg3: Arg[Name3],
    arg4: Arg[Name4],
  )(
    fn: (EvaluationContext[F], Source) ?=> (
      EvaluatedJValue[F],
      EvaluatedJValue[F],
      EvaluatedJValue[F],
      EvaluatedJValue[F],
    ) => F[EvaluatedJValue[F]],
  ): EvaluatedJValue.JFunction[F] =
    EvaluatedJValue.JFunction(stdSrc, 4, params => {
      bindArgs(params, Array(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
        compiletime.constValue[Name4] -> arg4.default,
      )).flatMap { case Array(a1, a2, a3, a4) =>
        fn(using params.ctx, params.src)(a1, a2, a3, a4)
      }
    })

  private inline def function5[
    Name1 <: String,
    Name2 <: String,
    Name3 <: String,
    Name4 <: String,
    Name5 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
    arg3: Arg[Name3],
    arg4: Arg[Name4],
    arg5: Arg[Name5],
  )(
    fn: (EvaluationContext[F], Source) ?=> (
      EvaluatedJValue[F],
      EvaluatedJValue[F],
      EvaluatedJValue[F],
      EvaluatedJValue[F],
      EvaluatedJValue[F],
    ) => F[EvaluatedJValue[F]],
  ): EvaluatedJValue.JFunction[F] =
    EvaluatedJValue.JFunction(stdSrc, 5, params => {
      bindArgs(params, Array(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
        compiletime.constValue[Name4] -> arg4.default,
        compiletime.constValue[Name5] -> arg5.default,
      )).flatMap { case Array(a1, a2, a3, a4, a5) =>
        fn(using params.ctx, params.src)(a1, a2, a3, a4, a5)
      }
    })

  private val jidentity = function1(Arg.x)(_.pure)
  private val jnull = EvaluatedJValue.JNull[F](stdSrc)
  private val jarray = EvaluatedJValue.JArray[F](stdSrc, IArray.empty)
  private val jtrue = EvaluatedJValue.JBoolean[F](stdSrc, true)
  private val jfalse = EvaluatedJValue.JBoolean[F](stdSrc, false)

  val members: Map[String, EvaluatedJValue[F]] = Map(
    "toString" -> function1(Arg.x)(x => ctx.singleLinePrint(x).map(EvaluatedJValue.JString(src, _))),
    "type" -> function1(Arg.x) { x =>
      EvaluatedJValue.JString(src, EvaluationContext.typeString(x)).pure
    },
    "length" -> function1(Arg.x) { x =>
      ctx.expect[
        EvaluatedJValue.JArray[F]
        | EvaluatedJValue.JString[F]
        | EvaluatedJValue.JObject[F]
        | EvaluatedJValue.JFunction[F]
      ](x).map {
        case e: EvaluatedJValue.JArray[F] => EvaluatedJValue.JNum[F](src, e.elements.size)
        case e: EvaluatedJValue.JString[F] => EvaluatedJValue.JNum[F](src, e.string.size)
        case e: EvaluatedJValue.JObject[F] => EvaluatedJValue.JNum[F](src, e.members.size)
        case e: EvaluatedJValue.JFunction[F] => EvaluatedJValue.JNum[F](src, e.numParams)
      }
    },
    "get" -> function4(Arg.x, Arg.f, Arg.default(jnull), Arg.inc_hidden(jtrue)) {
      (x, f, default, inc_hidden) =>
        (
          ctx.expect[EvaluatedJValue.JObject[F]](x),
          ctx.expect[String](f),
          ctx.expect[Boolean](inc_hidden),
        ).tupled.flatMap { (x, f, inc_hidden) =>
          x
            .members
            .get(f)
            .fold(default.pure) { m =>
              if !inc_hidden && m.isHidden then default.pure else m.value
            }
        }
    },
    "objectHas" -> function2(Arg.o, Arg.f) { (o, f) =>
      (ctx.expect[EvaluatedJValue.JObject[F]](o), ctx.expect[String](f)).mapN { (o, f) =>
        EvaluatedJValue.JBoolean(src, o.members.contains(f))
      }
    },
    "objectFields" -> function1(Arg.o) { (o) =>
      ctx.expect[EvaluatedJValue.JObject[F]](o).map { o =>
        // EvaluatedJValue.JArray(src, o.members().keys.toSeq.sorted) // BUG
        val keys = o.members
          .keys
          .map(EvaluatedJValue.JString[F](src, _))
          .toArray
          .sortBy(_.string)
        EvaluatedJValue.JArray(src, IArray.unsafeFromArray(keys))
      }
    },
    "flatMap" -> function2(Arg.func, Arg.arr) { (func, arr) =>
      (
        ctx.expect[EvaluatedJValue.JFunction[F]](func),
        ctx.expect[EvaluatedJValue.JArray[F]](arr),
      ).tupled.flatMap { (func, arr) =>
        for
          res <- Sync[F].delay(Array.ofDim[IArray[EvaluatedJValue[F]]](arr.elements.size))
          _ <- ((0 until arr.elements.size): Iterable[Int]).parTraverse { i =>
            val e = arr.elements(i)
            val params = EvaluatedJValue.JFunctionParameters(src, ctx, Seq(e), Seq.empty)
            for
              mapped <- func.fn(params)
              arr <- ctx.expect[EvaluatedJValue.JArray[F]](mapped)
              _ <- Sync[F].delay(res(i) = arr.elements)
            yield
              ()
          }
        yield
          EvaluatedJValue.JArray(src, IArray.unsafeFromArray(res.flatten))
      }
    },
    "uniq" -> function2(Arg.arr, Arg.keyF(jidentity)) { (arr, keyF) =>
      (ctx.expect[EvaluatedJValue.JFunction[F]](keyF), ctx.expect[EvaluatedJValue.JArray[F]](arr)).mapN { (keyF, arr) =>
        val elements =
          if keyF eq jidentity then
            arr.elements.distinct
          else
            arr.elements.distinctBy(e => keyF.fn(EvaluatedJValue.JFunctionParameters(src, ctx, Seq(e), Seq.empty)))
        EvaluatedJValue.JArray(src, elements)
      }
    },
    "trace" -> function2(Arg.str, Arg.rest) { (str, rest) =>
      ctx.expect[String](str).map { str =>
        val file = ctx.file
        val lineNum = src match
        case r: Source.Range => ":" + file.getLineCol(r.start)._1
        case _ => ""
        ConsoleLogger[F].info(s"TRACE: ${file.path}$lineNum: ${str}")
        rest
      }
    },
    "print" -> function2(Arg.str, Arg.rest(jnull)) { (rawStr, rest) =>
      ctx.singleLinePrint(rawStr).map { str =>
        ConsoleLogger[F].info(str)
        if rest eq jnull then rawStr
        else rest
      }
    },
    "runJob" -> function1(Arg.desc) { (desc) =>
      ctx.decode[JobDescription](desc)
        .flatMap(JobRunner.run(ctx, jobCache, src, _))
        .map(ctx.encode(src, _))
    },
  )

object Std:
  private val stdSrc = Source.Generated(SourceFile.std)

  def apply[F[_]: Async: ConsoleLogger: Logger: Parallel](
    ctx: EvaluationContext[F],
  ): Resource[F, EvaluatedJValue.JObject[F]] =
    for
      jobCache <- SQLiteJobCache[F](ctx.workspaceDir)
    yield
      EvaluatedJValue.JObject.static(
        stdSrc,
        ctx,
        new Std[F](jobCache).members
      )
