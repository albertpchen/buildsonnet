package buildsonnet

import cats.{Parallel, MonadError, Traverse}
import cats.effect.{Async, Deferred, Sync, Ref, Resource}
import cats.effect.std.Dispatcher
import cats.effect.syntax.all.given
import cats.syntax.all.given
import cats.instances.all.given

import buildsonnet.ast.{JParamList, JValue, Source, SourceFile}
import buildsonnet.evaluator.{EvaluationContext, EvaluatedJValue, EvaluationError, LazyValue, LazyObjectValue, eval}
import buildsonnet.evaluator.given Traverse[Iterable]
import buildsonnet.logger.ConsoleLogger
import buildsonnet.job.{JobCache, JobDescription, SQLiteJobCache, JobRunner}
import buildsonnet.bsp.{SocketConnection, BloopServer}

import java.nio.file.{Files, Path}

import org.typelevel.log4cats.Logger

import scala.language.dynamics

private class Std[F[_]: Async: ConsoleLogger: Logger: Parallel] private (
  initCtx: EvaluationContext[F],
  bloopServer: F[BloopServer[F]],
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
          if currPosArgs.nonEmpty && isGivenNamedArg then
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

  private var self: EvaluatedJValue.JObject[F] = null
  val members: F[Map[String, EvaluatedJValue[F]]] =
    for
      scalaProject <- LazyObjectValue(true, Sync[F].defer {
        val ctxWithStd = initCtx.bind("std", LazyValue.strict(self))
        eval(ctxWithStd)(JValue.reifyFile(
          "../resources/std.scala.Project.jsonnet",
          "std.scala.Project"
        ))
      })
    yield
      val members = Map(
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
              _ <- ((0 until arr.elements.size): Iterable[Int]).traverse { i =>
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
        "write" -> function2(Arg.pathName, Arg.contents) { (pathName, contents) =>
          (ctx.expect[String](pathName), ctx.expect[String](contents))
            .tupled
            .flatMap { (pathName, contents) =>
              val path =
                if pathName.startsWith("/") then
                  java.nio.file.Paths.get(pathName).normalize
                else
                  ctx.workspaceDir.resolve(pathName).normalize
              val parent = path.getParent
              if !java.nio.file.Files.exists(parent) && !parent.toFile.mkdirs then
                ctx.error(src, s"could not create parent directories for file '$path'")
              else if !java.nio.file.Files.isDirectory(parent) then
                ctx.error(src, s"file parent is not a directory '$path'")
              else if java.nio.file.Files.isDirectory(path) then
                ctx.error(src, s"file '$path' is a directory")
              else
                writeImpl(path, contents).map(p => EvaluatedJValue.JString(src, p.toString))
            }
        },
        "find" -> function3(Arg.dir, Arg.pattern, Arg.syntax(EvaluatedJValue.JString(stdSrc, "glob"))) { (dir, pattern, syntax) =>
          (
            ctx.expect[String](dir),
            ctx.expect[String](pattern),
            ctx.expect[String](syntax)
          ).tupled.flatMap { (dirName, pattern, syntax) =>
            import collection.JavaConverters.asScalaIteratorConverter
            val dirPath =
              if dirName.startsWith("/") then
                java.nio.file.Paths.get(dirName).normalize
              else
                ctx.workspaceDir.resolve(dirName).normalize
            if !java.nio.file.Files.exists(dirPath) then
              ctx.error(src, s"directory does not exist, got '$dirPath'")
            else if !java.nio.file.Files.isDirectory(dirPath) then
              ctx.error(src, s"filename is not a directory, got '$dirPath'")
            else
              for
                matcher <- syntax match
                  case "glob" | "regex" =>
                    java.nio.file.FileSystems.getDefault.getPathMatcher(s"$syntax:$pattern").pure
                  case _ =>
                    ctx.error(src, s"paths syntax must be either 'glob' or 'regex', got $syntax")
                elements <- Sync[F].delay {
                  Files
                    .find(dirPath, Integer.MAX_VALUE, (filePath, fileAttr) => {
                       fileAttr.isRegularFile() && matcher.matches(dirPath.relativize(filePath))
                    })
                    .iterator()
                    .asScala
                    .map(p => EvaluatedJValue.JString[F](src, p.toString))
                    .toArray
                }
              yield
                EvaluatedJValue.JArray(src, IArray.unsafeFromArray(elements))
          }
        },
        "startsWith" -> function2(Arg.a, Arg.b) { (a, b) =>
          (ctx.expect[String](a), ctx.expect[String](b)).mapN { (a, b) =>
            EvaluatedJValue.JBoolean(src, a.startsWith(b))
          }
        },
        "join" -> function2(Arg.sep, Arg.arr) { (sep, arr) =>
          (ctx.expect[String](sep), ctx.expect[EvaluatedJValue.JArray[F]](arr))
            .tupled
            .flatMap {
              case (sep, arr) if arr.elements.isEmpty => EvaluatedJValue.JString(src, "").pure
              case (sep, arr) if arr.elements.size == 1 => ctx.expect[EvaluatedJValue.JString[F]](arr.elements.head).widen
              case (sep, arr) =>
                ((0 until arr.elements.size): Iterable[Int])
                  .traverse(i => ctx.expect[String](arr.elements(i)))
                  .map(arr => EvaluatedJValue.JString(src, arr.mkString(sep)))
            }
        },
        "getenv" -> function1(Arg.varName) { (varName) =>
          ctx.expect[String](varName).flatMap { varName =>
            Sync[F].defer {
              try
                val value = System.getenv(varName)
                if value eq null then
                  ctx.error(src, s"environment variable \"$varName\" not set")
                else
                  EvaluatedJValue.JString(src, value).pure
              catch
                case e: java.lang.SecurityException =>
                  ctx.error(src, s"could not access environment variable \"$varName\": ${e.getMessage}")
            }
          }
        },
        "java" -> EvaluatedJValue.JObject.static(stdSrc, Map(
          "Dep" -> function3(Arg.org, Arg.name, Arg.version) { (org, name, version) =>
            (
              ctx.expect[EvaluatedJValue.JString[F]](org),
              ctx.expect[EvaluatedJValue.JString[F]](name),
              ctx.expect[EvaluatedJValue.JString[F]](version),
            ).mapN {
              (org, name, version) => EvaluatedJValue.JObject.static(stdSrc, Map(
                "org" -> org,
                "name" -> name,
                "version" -> version,
                "type" -> EvaluatedJValue.JString(src, "java"),
              ))
            }
          },
          "fetchJvm" -> function2(
            Arg.name,
            Arg.index(EvaluatedJValue.JString(
              stdSrc,
              "https://github.com/coursier/jvm-index/raw/master/index.json",
            ))
          ) { (name, index) =>
            import coursier.util.Task
            import coursier.cache.FileCache
            import coursier.cache.loggers.RefreshLogger
            import coursier.jvm.{JavaHome, JvmCache}
            import CoursierCatsInterop.given
            for
              name <- ctx.decode[String](name)
              index <- ctx.decode[String](index)
              ec <- Async[F].executionContext
              future = Sync[F].delay {
                val fileCache = FileCache[Task]()
                  .withLogger(RefreshLogger.create(System.out))
                val jvmCache = JvmCache()
                  .withCache(fileCache)
                  .withIndex(index)
                JavaHome()
                  .withCache(jvmCache)
                  .get(name)
                  .attempt
                  .future()(ec)
              }
              result <- Async[F].fromFuture(future).flatMap {
                case Left(e) => ctx.error(src, s"fetchJvm failure: ${e.getMessage}")
                case Right(f) => EvaluatedJValue.JString[F](src, f.toPath.normalize.toString).pure
              }
            yield
              result
          },
        )),
        "scala" -> EvaluatedJValue.JObject.static(stdSrc, Map(
          "Dep" -> function4(
            Arg.org,
            Arg.name,
            Arg.version,
            Arg.crossVersion(jnull),
          ) { (org, name, version, crossVersion) =>
            (
              ctx.expect[EvaluatedJValue.JString[F]](org),
              ctx.expect[EvaluatedJValue.JString[F]](name),
              ctx.expect[EvaluatedJValue.JString[F]](version),
              ctx.expect[EvaluatedJValue.JString[F] | EvaluatedJValue.JNull[F]](crossVersion),
            ).mapN {
              (org, name, version, crossVersion) => EvaluatedJValue.JObject.static(stdSrc, Map(
                "org" -> org,
                "name" -> name,
                "version" -> version,
                "type" -> EvaluatedJValue.JString(src, "scala"),
              ) ++ (if !crossVersion.isNull then Some("crossVersion" -> crossVersion) else None))
            }
          },
          "Project" -> scalaProject,
          "fetchDeps" -> function2(Arg.deps, Arg.withSources(jfalse)) { (deps, withSources) =>
            import coursier.{Classifier, Fetch, Type}
            import coursier.cache.FileCache
            import coursier.cache.loggers.RefreshLogger
            import CoursierCatsInterop.given
            (
              ctx.decode[List[CoursierDependency]](deps),
              ctx.expect[Boolean](withSources),
            ).tupled.flatMap { (deps, withSources) =>
              val logger = RefreshLogger.create(ConsoleLogger[F].outStream)
              val cache = FileCache[F]()
              val base =
                Fetch[F](cache)
                  .withDependencies(deps.map(_.toDependency))
                  .withClasspathOrder(true)
              val withOptions =
                if withSources then
                  base
                    .addClassifiers(Classifier.sources)
                    .addArtifactTypes(Type.source)
                else
                  base
              withOptions
                .io
                .map(files => ctx.encode(src, files.map(_.toPath.normalize.toString)))
            }
          },
          "compile" -> function1(Arg.project) { (project) =>
            for
              project <- ctx.decode[BloopConfig.RecursiveProject](project)
              _ <- BloopConfig.write(ctx, project)
              bloopServer <- bloopServer
              either <- bloopServer.compile(project.name)
              result <- either match
                case Right(_) => EvaluatedJValue.JNull[F](src).pure
                case Left(msg) => ctx.error(src, msg)
            yield
              result
          },
          "run" -> function5(
            Arg.project,
            Arg.main,
            Arg.args(jarray),
            Arg.jvmOptions(jarray),
            Arg.environmentVariables(jarray),
          ) { (project, main, args, jvmOptions, environmentVariables) =>
            (
              ctx.decode[BloopConfig.RecursiveProject](project),
              ctx.decode[String](main),
              ctx.decode[List[String]](jvmOptions),
              ctx.decode[List[String]](args),
              ctx.decode[List[String]](environmentVariables),
            ).tupled.flatMap { (project, main, jvmOptions, args, environmentVariables) =>
              for
                _ <- BloopConfig.write(ctx, project)
                bloopServer <- bloopServer
                either <- bloopServer.run(project.name, ch.epfl.scala.bsp.ScalaMainClass(
                  `class` = main,
                  arguments = args,
                  environmentVariables = environmentVariables,
                  jvmOptions = jvmOptions,
                ))
                result <- either match
                  case Right(_) => EvaluatedJValue.JNull[F](src).pure
                  case Left(msg) => ctx.error(src, msg)
              yield
                result
            }
          },
          "classpath" -> function1(Arg.project) { project =>
            for
              project <- ctx.decode[BloopConfig.RecursiveProject](project)
              _ <- BloopConfig.write(ctx, project)
              bloopServer <- bloopServer
              either <- bloopServer.jvmRunEnvironment(project.name)
              result <- either match
                case Right(env) =>
                  val classpath = env.items.head.classpath
                  val arr = Array.ofDim[EvaluatedJValue.JString[F]](classpath.size)
                  classpath.zipWithIndex.foreach { (item, i) =>
                    val file = java.nio.file.Paths.get(new java.net.URI(item))
                    arr(i) = EvaluatedJValue.JString(src, file.toString)
                  }
                  EvaluatedJValue.JArray(src, IArray.unsafeFromArray(arr)).pure
                case Left(msg) => ctx.error(src, msg)
            yield
              result
          },
          "mainClasses" -> function1(Arg.project) { project =>
            for
              project <- ctx.decode[BloopConfig.RecursiveProject](project)
              _ <- BloopConfig.write(ctx, project)
              bloopServer <- bloopServer
              either <- bloopServer.mainClasses(project.name)
              result <- either match
                case Right(env) =>
                  val classes = env.items.head.classes
                  val arr = Array.ofDim[EvaluatedJValue.JString[F]](classes.size)
                  classes.zipWithIndex.foreach { (item, i) =>
                    arr(i) = EvaluatedJValue.JString(src, item.`class`)
                  }
                  EvaluatedJValue.JArray(src, IArray.unsafeFromArray(arr)).pure
                case Left(msg) => ctx.error(src, msg)
            yield
              result
          },
        )),
      )
      self = EvaluatedJValue.JObject.static[F](stdSrc, members)
      members

object Std:
  private val stdSrc = Source.Generated(SourceFile.std)
  private case class CtxImpl[F[_]](
    self: Option[EvaluatedJValue.JObject[F]],
    `super`: Option[EvaluatedJValue.JObject[F]],
    scopeArr: Array[(String, LazyValue[F])],
    val workspaceDir: Path,
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

    def withSuper(obj: Option[EvaluatedJValue.JObject[F]]): EvaluationContext[F] =
      this.copy(`super` = obj)

    def `import`(src: Source, file: String): F[EvaluatedJValue[F]] = ???
    def importStr(src: Source, file: String): F[EvaluatedJValue.JString[F]] = ???


  def ctx[F[_]](workspaceDir: Path)(using MonadError[F, Throwable]): EvaluationContext[F] =
    CtxImpl(
      None,
      None,
      Array("workspace" -> LazyValue.strict(EvaluatedJValue.JString[F](stdSrc, workspaceDir.toString))),
      workspaceDir,
    )

  def writeImpl[F[_]: Sync](path: Path, contents: String): F[Path] =
    Sync[F].delay {
      if !Files.exists(path) || !fileMatchesContents(path.toFile, contents) then
        path.getParent.toFile.mkdirs
        Files.write(path, contents.getBytes())
      path
    }

  private def fileMatchesContents(file: java.io.File, contents: String): Boolean =
    val fileStream = new java.io.FileInputStream(file)
    val contentsStream = new java.io.ByteArrayInputStream(contents.getBytes())
    var cFile: Int = -1
    var cContents: Int = -1
    while
      cFile = fileStream.read()
      cContents = contentsStream.read()
      (cFile == cContents) && (cFile != -1 || cContents != -1)
    do ()
    fileStream.close()
    contentsStream.close()
    cFile == cContents

  private def lazyResource[F[_]: Async, T](resource: Resource[F, T]): Resource[F, F[T]] =
    for
      deferred <- Resource.eval(Deferred[F, F[Unit]])
      value <- Resource.eval {
        resource.allocated.flatMap { (value, finalize) =>
          deferred.complete(finalize).as(value)
        }.memoize
      }
      _ <- Resource.onFinalize(deferred.tryGet.flatMap(_.fold(().pure)(identity)))
    yield
      value

  def apply[F[_]: Async: ConsoleLogger: Logger: Parallel](
    ctx: EvaluationContext[F],
  ): Resource[F, EvaluatedJValue.JObject[F]] =
    for
      jobCache <- SQLiteJobCache[F](ctx.workspaceDir)
      bloopServerEither <- lazyResource(SocketConnection.connectToLauncher[F](
        bloopVersion = "1.5.0",
        bloopPort = 8213,
        logStream = System.out,
      ).flatMap(BloopServer(ctx.workspaceDir, _, maxConcurrentServiceWorkers = 1)))
      bloopServer = bloopServerEither.flatMap {
        case Left(error) => ctx.error(stdSrc, error)
        case Right(server) => server.pure
      }
      std = new Std[F](ctx, bloopServer, jobCache)
      _ <- Resource.eval(std.members)
    yield
      std.self
