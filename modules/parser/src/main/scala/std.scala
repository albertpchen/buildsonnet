package root

import concurrent.{ExecutionContext, Future}

object Std:
  private def bindArgs(
    argNames: Seq[(String, Option[EvaluatedJValue])],
    ctx: EvaluationContext,
    params: EvaluatedJFunctionParameters
  ): Array[EvaluatedJValue] =
    val positionalArgs = params.positionalArgs
    val namedArgs = params.namedArgs.toMap
    val numGivenArgs = positionalArgs.size + namedArgs.size
    val result = Array.ofDim[EvaluatedJValue](argNames.size)
    var i = 0
    for
      (argName, defaultOpt) <- argNames
    do
      result(i) =
        if i < positionalArgs.size then
          if namedArgs.contains(argName) then ctx.error(positionalArgs(i).src, s"multiple values provided for argument $argName")
          positionalArgs(i)
        else if namedArgs.contains(argName) then
          namedArgs(argName)
        else if defaultOpt.isDefined then
          defaultOpt.get
        else
         ctx.error(params.src, s"no argument provided for $argName")
      i += 1
    result

  type ArgTuple[ArgNames] <: Tuple = ArgNames match
    case EmptyTuple => EmptyTuple
    case (arg *: tail) => EvaluatedJValue *: ArgTuple[tail]

  object temp:
    opaque type Arg[Name] = Option[EvaluatedJValue]

    import scala.language.dynamics
    object Arg extends Dynamic:
      def selectDynamic(name: String): Arg[name.type] = None
      def applyDynamic(name: String)(default: EvaluatedJValue): Arg[name.type] = Some(default)
    extension [T](arg: Arg[T])
      def default: Option[EvaluatedJValue] = arg

  val Arg = temp.Arg
  type Arg[Name] = temp.Arg[Name]

  private inline def function0(
    fn: (EvaluationContext, Source) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(Source.Generated, 0, (applyCtx, params) => fn(applyCtx, params.src))

  private inline def function1[Name1 <: String](
    arg1: Arg[Name1],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(Source.Generated, 1, (applyCtx, params) => {
      val Array(a1) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default
      ), applyCtx, params)
      fn(applyCtx, params.src, a1)
    })

  private inline def function2[
    Name1 <: String,
    Name2 <: String,
  ](
    arg1: Arg[Name1],
    arg2: Arg[Name2],
  )(
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction(Source.Generated, 2, (applyCtx, params) => {
      val Array(a1, a2) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
      ), applyCtx, params)
      fn(applyCtx, params.src, a1, a2)
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
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction( Source.Generated, 3, (applyCtx, params) => {
      val Array(a1, a2, a3) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
      ), applyCtx, params)
      fn(applyCtx, params.src, a1, a2, a3)
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
    fn: (EvaluationContext, Source, EvaluatedJValue, EvaluatedJValue, EvaluatedJValue, EvaluatedJValue) => EvaluatedJValue,
  ): EvaluatedJValue.JFunction =
    EvaluatedJValue.JFunction( Source.Generated, 4, (applyCtx, params) => {
      val Array(a1, a2, a3, a4) = bindArgs(Seq(
        compiletime.constValue[Name1] -> arg1.default,
        compiletime.constValue[Name2] -> arg2.default,
        compiletime.constValue[Name3] -> arg3.default,
        compiletime.constValue[Name4] -> arg4.default,
      ), applyCtx, params)
      fn(applyCtx, params.src, a1, a2, a3, a4)
    })

  private def makeObject(
    ctx: EvaluationContext,
    staticMembers: EvaluationContext => Map[String, EvaluatedJValue | LazyObjectValue],
  ): EvaluatedJValue.JObject =
    var objCtx: ObjectEvaluationContext = null
    val obj: EvaluatedJValue.JObject = EvaluatedJValue.JObject(
      Source.Generated,
      EvaluatedJObject.static(() => objCtx, staticMembers)
    )
    objCtx = EvaluationContext.ObjectImp(
      ctx.bloopServer,
      obj,
      collection.immutable.Queue.empty,
      ctx,
      Map.empty,
      List.empty,
    )
    obj

  type JFunction1[T] = (EvaluationContext, Source, EvaluatedJValue) => T

  val toStringImp: JFunction1[EvaluatedJValue.JString] = (ctx, src, a) => {
    val value = a match
    case EvaluatedJValue.JString(_, str) => str
    case _ => a.manifest(ctx).toString
    EvaluatedJValue.JString(src, value)
  }

  private val jnull = EvaluatedJValue.JNull(Source.Generated)
  private val jtrue = EvaluatedJValue.JBoolean(Source.Generated, true)
  private val jidentity = function1(Arg.x) { (ctx, src, x) => x }
  def obj(ctx: EvaluationContext) = makeObject(ctx, ctx => Map(
    "toString" -> function1(Arg.x)(toStringImp),
    "type" -> function1(Arg.x) { (ctx, src, x) =>
      x match
      case x: EvaluatedJValue.JFuture =>
        given concurrent.ExecutionContext = ctx.executionContext
        x.future.map { x =>
          EvaluatedJValue.JString(src, EvaluationContext.typeString(x))
        }.toJValue
      case _ =>
        EvaluatedJValue.JString(src, EvaluationContext.typeString(x))
    },
    "length" -> function1(Arg.x) { (ctx, src, x) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[
        EvaluatedJValue.JArray
        | EvaluatedJValue.JString
        | EvaluatedJValue.JObject
        | EvaluatedJValue.JFunction
      ](x).map {
        case e: EvaluatedJValue.JArray => EvaluatedJValue.JNum(src, e.elements.size)
        case e: EvaluatedJValue.JString => EvaluatedJValue.JNum(src, e.str.size)
        case e: EvaluatedJValue.JObject => EvaluatedJValue.JNum(src, e.members().size)
        case e: EvaluatedJValue.JFunction => EvaluatedJValue.JNum(src, e.numParams)
      }.toJValue
    },
    "get" -> function4(Arg.x, Arg.f, Arg.default(jnull), Arg.inc_hidden(jtrue)) {
      (ctx, src, o, f, default, i) =>
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.expectObject(o).zip(ctx.expectString(f)).zip(ctx.expectBoolean(i)).map {
          case ((members, field), inc_hidden) =>
            members.members().get(field.str).fold(default) { m =>
              if !inc_hidden.value && m.isHidden then default else m.evaluated
            }
        }.toJValue
    },
    "objectHas" -> function2(Arg.o, Arg.f) { (ctx, src, o, f) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectObject(o).zip(ctx.expectString(f)).map { (o, f) =>
        EvaluatedJValue.JBoolean(src, o.members().contains(f.str))
      }.toJValue
    },
    "objectFields" -> function1(Arg.o) { (ctx, src, o) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectObject(o).map { o =>
        // EvaluatedJValue.JArray(src, o.members().keys.toSeq.sorted) // BUG
        val keys = o.members()
          .keys
          .map(EvaluatedJValue.JString(src, _): EvaluatedJValue.JString)
          .toSeq
          .sortBy(_.str)
        EvaluatedJValue.JArray(src, keys)
      }.toJValue
    },
    "flatMap" -> function2(Arg.func, Arg.arr) { (ctx, src, func, arr) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectFunction(func).zip(ctx.expectArray(arr)).flatMap { (func, arr) =>
        arr.elements.foldLeft(Future(Seq.empty[Seq[EvaluatedJValue]])) { (acc, e) =>
          val params = EvaluatedJFunctionParameters(src, Seq(e), Seq.empty)
          acc.zip(ctx.expectArray(func.fn(ctx, params))).map((acc, e) => e.elements +: acc)
        }.map(a => EvaluatedJValue.JArray(src, a.reverse.flatten))
      }.toJValue
    },
    "uniq" -> function2(Arg.arr, Arg.keyF(jidentity)) { (ctx, src, arr, keyF) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectFunction(keyF).zip(ctx.expectArray(arr)).map { (keyF, arr) =>
        val elements =
          if keyF eq jidentity then arr.elements.distinct
          else arr.elements.distinctBy(e => keyF.fn(ctx, EvaluatedJFunctionParameters(src, Seq(e), Seq.empty)))
        EvaluatedJValue.JArray(src, elements)
      }.toJValue
    },
    "trace" -> function2(Arg.str, Arg.rest) { (ctx, src, str, rest) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectString(str).map { str =>
        val file = ctx.file
        val lineNum = src match
        case Source.Range(start, _) => ":" + file.getLineCol(start)._1
        case _ => ""
        println(s"TRACE: ${file.path}$lineNum: ${str.str}")
        rest
      }.toJValue
    },
    "print" -> function2(Arg.str, Arg.rest(jnull)) { (ctx, src, str, rest) =>
      given concurrent.ExecutionContext = ctx.executionContext
      println(toStringImp(ctx, src, str).str)
      if rest eq jnull then str
      else rest
    },
    "workspace" -> EvaluatedJValue.JPath(Source.Generated, ctx.workspaceDir),
    "runJob" -> function1(Arg.desc) { (ctx, src, desc) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.decode[JobDescription](desc).flatMap(ctx.runJob(src, _)).toJValue
    },
    "source" -> function1(Arg.pathName) { (ctx, src, pathName) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[EvaluatedJValue.JString](pathName).map { pathName =>
        import JobRunner.resolvePath
        if pathName.str.startsWith("/") then ctx.error(src, s"cannot source an absolute path, got $pathName")
        EvaluatedJValue.JPath(src, ctx.resolvePath(pathName.str))
      }.toJValue
    },
    "write" -> function2(Arg.pathName, Arg.contents) { (ctx, src, pathName, contents) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[EvaluatedJValue.JString | EvaluatedJValue.JPath](pathName).flatMap { pathName =>
        ctx.expectString(contents).map { contents =>
          val path =
            pathName match
            case str: EvaluatedJValue.JString =>
              if str.str.startsWith("/") then
                java.nio.file.Paths.get(str.str)
              else
                ctx.workspaceDir.resolve(str.str)
            case path: EvaluatedJValue.JPath => path.path
          EvaluatedJValue.JPath(src, JobRunner.write(path, contents.str))
        }
      }.toJValue
    },
    "startsWith" -> function2(Arg.a, Arg.b) { (ctx, src, a, b) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectString(a).zip(ctx.expectString(b)).map { (a, b) =>
        EvaluatedJValue.JBoolean(src, a.str.startsWith(b.str))
      }.toJValue
    },
    "getenv" -> function1(Arg.varName) { (ctx, src, varName) =>
      given concurrent.ExecutionContext = ctx.executionContext
      ctx.expectType[EvaluatedJValue.JString](varName).map { varNamex =>
        val varName = varNamex.str
        try
          val value = System.getenv(varName)
          if value eq null then
            ctx.error(src, s"environment variable \"$varName\" not set")
          else
            EvaluatedJValue.JString(src, value)
        catch
          case e: java.lang.SecurityException => ctx.error(src, s"could not access environment variable \"$varName\": ${e.getMessage}")
      }.toJValue
    },
    "scala" -> makeObject(ctx.bind("std", JValue.JSelf(Source.Generated)), ctx => Map(
      "Project" -> {
        val contents = JValue.readFile("../resources/bloop.jsonnet")
        val newCtx = ctx.withFile(SourceFile("std.scala.Project", contents))
        LazyValue(newCtx, JValue.reifyFile("../resources/bloop.jsonnet"), true)
      },
      "cs" -> function1(Arg.deps) { (ctx, src, deps) =>
        import coursier.{Dependency, Fetch, Module, ModuleName, Organization}
        import coursier.cache.FileCache
        import coursier.cache.loggers.RefreshLogger
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.decode[Seq[CoursierDependency]](deps).flatMap { deps =>
          Fetch()
            .withDependencies(deps.map(_.toDependency))
            // .addDependencies(params.deps.map(_.toDependency)) // BUG
            .withCache(
              FileCache().withLogger(RefreshLogger.create(System.out))
            )
            .future()
            .map { files =>
              EvaluatedJValue.JArray(src, files.map(a => EvaluatedJValue.JPath(src, a.toPath)))
            }
        }.toJValue
      },
      "compile" -> function1(Arg.project) { (ctx, src, project) =>
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.decode[Config.RecursiveProject](project).flatMap { project =>
          Config.write(ctx, project)
          ctx.bloopServer.compile(project.name).map { res =>
            val result = res.getStatusCode() match
            case ch.epfl.scala.bsp4j.StatusCode.OK => "ok"
            case ch.epfl.scala.bsp4j.StatusCode.ERROR => "error"
            case ch.epfl.scala.bsp4j.StatusCode.CANCELLED => "cancelled"
            EvaluatedJValue.JString(src, result)
          }
        }.toJValue
      },
      "classpath" -> function1(Arg.project) { (ctx, src, project) =>
        given concurrent.ExecutionContext = ctx.executionContext
        ctx.decode[Config.RecursiveProject](project).flatMap { project =>
          import scala.jdk.CollectionConverters.given
          import ch.epfl.scala.bsp4j.StatusCode
          Config.write(ctx, project)
          ctx.bloopServer.jvmRunEnvironment(project.name).map {
            case Right(env) =>
              val strings = env.getItems.get(0).getClasspath.asScala.map { item =>
                val file = java.nio.file.Paths.get(new java.net.URI(item))
                EvaluatedJValue.JPath(src, file)
              }.toSeq
              EvaluatedJValue.JArray(src, strings)
            case Left(StatusCode.ERROR) => ctx.error(src, s"compilation for '${project.name}' failed")
            case Left(StatusCode.CANCELLED) => ctx.error(src, s"compilation for '${project.name}' was cancelled")
          }
        }.toJValue
      },
    ))
  ))
