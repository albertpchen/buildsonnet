package root

case class CmdlineArgs(
  directory: Option[String],
  bloopPort: Int,
  buildCmd: String,
  buildArgs: String,
)

object Buildsonnet:
  val usage =
    """usage: buildsonnet [<builsonnet-options>] <command> [<args>]
      |
      |buildsonnet will search for a build.jsonnet file starting from the
      |current directory and searching upwards. This file should evaluate to a
      |jsonnet object. <command> should be the name of a field in the object.
      |This field should be a function that takes an array of strings. <args>
      |is passed verbatim to this function. All buildsonnet options must come
      |before <command> and <args>.
      |
      |buildsonnet options:
      |   --directory | -C <directory>
      |      find build root starting from a different directory, defaults to current directory
      |   --bloop-port | -p <bloop-port>
      |      the port number to start the bloop server on
      |""".stripMargin

  def main(args: Array[String]): Unit =
    var i = 0
    var doExit = false
    val argsOpt = args.lift
    var directory: String = null
    var bloopPort: Int = 8212
    var error: String = null
    var stop = false
    while i < args.size && error == null && !stop do
      args(i) match
      case "-C" | "--directory" =>
        argsOpt(i + 1).fold {
          error = "missing directory argument"
        } { dir =>
          directory = dir
          i += 1
        }
      case "-p" | "--bloop-port" =>
        argsOpt(i + 1).fold {
          error = "missing bloop-port argument"
        } { str =>
          try bloopPort = str.toInt
          catch
            case _: java.lang.NumberFormatException => error = s"--bloop-port expects a number"
          finally
            i += 1
        }
      case "--exit" => doExit = true
      case bad if bad.startsWith("--") => error = s"invalid argument: $bad"
      case cmd =>
        i -= 1
        stop = true
      i += 1

    if error != null then
      System.err.println(error)
      println(usage)
      System.exit(1)

    if i >= args.size then
      System.err.println("missing build command")
      println(usage)
      System.exit(1)
    val buildCommand = args(i)
    val buildArgs: Seq[String] = args.slice(i + 1, args.size)

    import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService}
    import scala.concurrent.duration.Duration
    import scala.util.{Failure, Success}

    var currPath = java.nio.file.Paths.get("").toAbsolutePath
    var buildFile: java.nio.file.Path = null
    while currPath != null && buildFile == null do
      val tryPath = currPath.resolve("build.jsonnet")
      if java.nio.file.Files.exists(tryPath) then
        buildFile = tryPath
      currPath = currPath.getParent

    if buildFile == null then
      System.err.println("could not find build.jsonnet file in current directory or in any parent directory")
      System.exit(1)

    val source = scala.io.Source.fromFile(buildFile.toFile).getLines.mkString("\n")
    val sourceFile = SourceFile(buildFile.toString, source)
    val parser = Parser.parserFile
    parser.parseAll(source) match
      case Left(error) =>
        System.err.println("syntax error at: " + error.toString)
        System.exit(1)
      case Right(ast) =>
        val exec = java.util.concurrent.Executors.newCachedThreadPool()
        given ExecutionContextExecutorService = ExecutionContext.fromExecutorService(exec)
        val withoutStd = EvaluationContext(
          file = sourceFile,
          bloopPort = bloopPort,
        )
        val ctx = withoutStd.bindEvaluated("std", Std.obj(withoutStd))
        val result: Either[EvaluationError, Unit] =
          try
            val buildValue = evalUnsafe(ctx)(
              JValue.JGetField(Source.Generated, ast, buildCommand))
            buildValue match
            case future: EvaluatedJValue.JFuture =>
              val applied = future.future.map {
                case fn: EvaluatedJValue.JFunction =>
                  val params = EvaluatedJFunctionParameters(fn.src, Seq(JValue.JArray(Source.Generated, buildArgs.map(JValue.JString(Source.Generated, _)))), Seq.empty)
                  fn.fn(ctx, params)
                case e => e
              }
              Right(Await.result(applied, Duration.Inf).await(ctx))
            case fn: EvaluatedJValue.JFunction =>
              val params = EvaluatedJFunctionParameters(fn.src, Seq(JValue.JArray(Source.Generated, buildArgs.map(JValue.JString(Source.Generated, _)))), Seq.empty)
              val applied = fn.fn(ctx, params)
              Right(applied.await(ctx))
            case e =>
              Right(e.await(ctx))
          catch
            case err: EvaluationError => Left(err)
          finally
            Await.result(ctx.bloopServer.shutdown(), Duration.Inf)
            summon[ExecutionContextExecutorService].shutdown()
        result match
          case Left(err) =>
            System.err.println(err)
            System.exit(1)
          case Right(_) =>
            System.exit(0)