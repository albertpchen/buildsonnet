package buildsonnet

import buildsonnet.ast.{Parser, SourceFile, JValue, Source}
import buildsonnet.evaluator.{EvaluationContext, EvaluatedJValue, EvaluationError, LazyValue}
import buildsonnet.logger.ConsoleLogger

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.effect.std.Console
import cats.syntax.all.given

import concurrent.duration.DurationInt

import com.monovore.decline.{Command, Opts}
import com.monovore.decline.effect.CommandIOApp

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.{Files, Path}


object Options:
  val directory = Opts.option[Path](
    "directory",
    short = "C",
    metavar = "directory",
    help = "find build root starting from a different directory, defaults to current directory",
  ).withDefault(java.nio.file.Paths.get(""))

  val bloopPort = Opts.option[Int](
    "bloop-port",
    short = "p",
    metavar = "port-number",
    help = "the port number to start the bloop server on",
  ).withDefault(8212)

  val runCommand = Opts.subcommand(Command(
    name = "run",
    header = "run a build command defined in the root build.jsonnet file",
  )((directory, bloopPort).tupled))

object Buildsonnet extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val interpreted = args.takeWhile(_ != "--")
    val uninterpreted =
      if args.contains("--") then
        args.dropWhile(_ != "--").drop(1)
      else
        List.empty

    CommandIOApp.run(
      name = "buildsonnet",
      header =
        """buildsonnet will search for a build.jsonnet file starting from the
          |current directory and searching upwards. This file should evaluate to a
          |jsonnet object. <command> should be the name of a field in the object.
          |This field should be a function that takes an array of strings. <args>
          |is passed verbatim to this function. All buildsonnet options must come
          |before <command> and <args>.
        """.stripMargin,
      helpFlag = true,
      version = Some("0.0.1")
    )(main(interpreted, uninterpreted), interpreted)

  final case class CmdlineError(message: String) extends Exception(message)

  def main(interpreted: List[String], uninterpreted: List[String]): Opts[IO[ExitCode]] =
    Options.runCommand.map { (directory, bloopPort) =>
      var buildFile: Path = null
      var currPath = directory.toAbsolutePath
      while currPath != null && buildFile == null do
        val tryPath = currPath.resolve("build.jsonnet")
        if Files.exists(tryPath) then
          buildFile = tryPath
        currPath = currPath.getParent

      val run =
        for
          uninterpreted <-
            if uninterpreted.isEmpty then IO.raiseError(new CmdlineError("missing build command argument"))
            else uninterpreted.pure[IO]
          buildFile <-
            if buildFile == null then IO.raiseError(new CmdlineError("could not find build.jsonnet file in current directory or in any parent directory"))
            else buildFile.pure[IO]
          source = scala.io.Source.fromFile(buildFile.toFile).getLines.mkString("\n")
          sourceFile = SourceFile(buildFile.toString, source)
          buildObject <- Parser(sourceFile).parseFile.fold(
            { error =>
              import cats.syntax.all.catsSyntaxOrder
              val offset = error.expected.map(_.offset).toList.max
              val (line, col) = sourceFile.getLineCol(offset)
              IO.raiseError(new CmdlineError(s"syntax error at ${scala.Console.UNDERLINED}${sourceFile.path}${scala.Console.RESET}:$line:$col"))
            },
            _.pure[IO]
          )
          buildCommand = uninterpreted(0)
          buildArgs = uninterpreted.drop(1)
          rootJValue = buildCommand
            .split('.')
            .foldLeft(buildObject)(JValue.JGetField(Source.empty, _, _))
          workspaceDir = buildFile.getParent
          given ConsoleLogger[IO] = ConsoleLogger.default[IO]("buildsonnet")
          given Logger[IO] = Slf4jLogger.getLogger[IO]
          _ <- Std.ctx[IO](workspaceDir, bloopPort, bloopVersion = "1.5.0").use { ctx =>
            for
              buildValue <- buildsonnet.evaluator.eval(ctx)(rootJValue)
              result <- buildValue match
                case fn: EvaluatedJValue.JFunction[IO] =>
                  val params = EvaluatedJValue.JFunctionParameters[IO](
                    fn.src,
                    ctx,
                    Seq(EvaluatedJValue.JArray(Source.empty, IArray.unsafeFromArray(buildArgs.map(EvaluatedJValue.JString(Source.empty, _)).toArray))),
                    Seq.empty,
                  )
                  fn.fn(params)
                case e if buildArgs.nonEmpty =>
                  IO.raiseError(new CmdlineError(s"build command $buildCommand is not a function"))
                case e => e.pure[IO]
              prettyResult <- ctx.prettyPrint(result)
              _ <- ConsoleLogger[IO].stdout(prettyResult)
            yield ()
          }
        yield ExitCode.Success
      run.handleErrorWith {
        case CmdlineError(msg) =>
          Console[IO].error(msg).as(ExitCode.Error)
        case e: EvaluationError =>
          Console[IO].error(e.toString).as(ExitCode.Error)
        case e => e.raiseError
      }
    }
