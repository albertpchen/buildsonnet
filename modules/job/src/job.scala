package buildsonnet.job

import buildsonnet.ast.{Source}
import buildsonnet.evaluator.{EvaluationContext}
import buildsonnet.job.syntax.*
import buildsonnet.logger.ConsoleLogger

import cats.effect.{Async, Sync}
import cats.effect.std.Dispatcher
import cats.syntax.all.given
import cats.Parallel

import collection.JavaConverters.mapAsScalaMapConverter

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, Paths}
import java.io.{BufferedReader, InputStreamReader}

import org.typelevel.log4cats.Logger

import scala.concurrent.duration.Duration

case class JobDescription(
  cmdline: Seq[String],
  envVars: Option[Map[String, String]],
  outputFiles: Option[Seq[String]],
  inputFiles: Seq[String],
  stdin: Option[String],
  directory: Option[String],
  fail: Option[Boolean],
)

case class JobKey(
  cmdline: Seq[String],
  envVars: Seq[(String, String)],
  inputFiles: Seq[(Path, String)],
  stdin: String,
  directory: Path,
)

case class JobValue(
  outputFiles: Seq[(Path, String)],
  stdout: String,
  stderr: String,
  exitCode: Int,
)

case class Job(
  stdout: String,
  stderr: String,
  outputs: Seq[Path],
  exitCode: Int,
)

trait JobCache[F[_]]:
  def get(key: JobKey): F[Option[JobValue]]
  def insert(key: JobKey, value: JobValue): F[Unit]

object JobRunner:
  private val charset = "utf-8"
  private given fileTimeOrdering: math.Ordering[FileTime] = new math.Ordering[FileTime]:
    def compare(x: FileTime, y: FileTime): Int =
      x.compareTo(y)

  import fileTimeOrdering.mkOrderingOps

  private def resolveCommand(cmd: String, path: Seq[String], dir: Path): Option[String] =
    if cmd.startsWith("/") then
      val path = Paths.get(cmd)
      return if Files.exists(path) then Some(path.toString) else None
    var result = Option.empty[String]
    for p <- path if result.isEmpty do
      val path =
        if p.startsWith("/") then Paths.get(s"$p/$cmd")
        else dir.resolve(p).resolve(cmd)
      if Files.exists(path) then result = Some(path.normalize().toString)
    return result

  def run[F[_]: Async: ConsoleLogger: Logger: Parallel](
    ctx: EvaluationContext[F],
    cache: JobCache[F],
    src: Source,
    desc: JobDescription,
  ): F[Job] = for
    _ <-
      if desc.directory.exists(_.startsWith("/")) then
        ctx.error(src, s"job directory may not be an absolute path, got ${desc.directory.get}")
      else ().pure
    _ <-
      desc.outputFiles.find(_.startsWith("/")).fold(().pure) { output =>
        ctx.error(src, s"job output file may not be an absolute path, got $output")
      }
    // _ <-
    //   desc.inputFiles.find(_.startsWith("/")).fold(().pure) { output =>
    //     ctx.error(src, s"job input file may not be an absolute path, got $output")
    //   }
    inputPaths <- Sync[F].defer {
      // TODO: handle InvalidPathException
      val paths = desc.inputFiles.map(ctx.workspaceDir.resolve)
      paths
        .find(path => !Files.exists(path))
        .fold(paths.distinct.sorted.pure) { path =>
          ctx.error(src, s"job input file does not exist, $path")
        }
    }
    _ <-
      if desc.cmdline.size <= 0 then ctx.error(src, s"job cmdline must be a non-empty list")
      else ().pure
    // TODO: handle InvalidPathException
    outputPaths <- Sync[F].delay {
      desc
        .outputFiles
        .getOrElse(Seq.empty)
        .map(ctx.workspaceDir.resolve)
        .distinct
        .sorted
    }
    // TODO: handle InvalidPathException
    directory <- Sync[F].delay(desc.directory.fold(ctx.workspaceDir)(ctx.workspaceDir.resolve(_)))
    cmd <- Sync[F].defer {
      val path = desc
        .envVars
        .fold(System.getenv().asScala)(identity)
        .get("PATH")
        .fold(Seq.empty)(_.split(":").toSeq) :+ "."
      resolveCommand(desc.cmdline.head, path, directory)
        .fold(ctx.error(src, s"could not resolve command \"${desc.cmdline.head}\" in path $path"))(
          _.pure,
        )
    }
    cmdline = (cmd +: desc.cmdline.tail).toArray
    inputPathHashes <- inputPaths.traverse(_.md5Hash(charset))
    jobKey = JobKey(
      cmdline = cmdline,
      envVars = desc.envVars.fold(System.getenv().asScala.toList.sorted)(_.toList.sorted),
      inputFiles = inputPaths.zip(inputPathHashes),
      stdin = desc.stdin.getOrElse(""),
      directory = directory.relativize(ctx.workspaceDir),
    )
    _ <- Logger[F].info(s"looking for key $jobKey in cache")
    cached <- cache.get(jobKey).flatMap {
      case None =>
        Logger[F].info(s"no value found in cache").as(None)
      case Some(jobValue) =>
        Sync[F].defer {
          val paths = jobValue.outputFiles.map { (pathName, hash) =>
            ctx.workspaceDir.resolve(pathName) -> hash
          }
          val expectedOutputPaths = outputPaths.toSet
          var staleReason: String | Null = null
          val diff = expectedOutputPaths.diff(paths.map(_._1).toSet)
          if diff.nonEmpty then
            staleReason = s"missing expected output paths:\n  ${diff.toSeq.sorted.mkString("\n  ")}"
          else
            paths.exists { (path, hash) =>
              if !path.exists then
                staleReason = s"output path does not exist: \"$path\" "
                true
              else if path.md5HashUnsafe(charset) != hash then
                staleReason = s"output path hash is different from last recorded hash: \"$path\""
                true
              else false
            }
          staleReason match
          case reason: String =>
            Logger[F].info(s"got stale value $jobValue from cache: $reason").as(None)
          case null =>
            Logger[F]
              .info(s"got valid value $jobValue from cache")
              .as(
                Some(
                  Job(
                    jobValue.stdout,
                    jobValue.stderr,
                    paths.map { (path, _) =>
                      ctx.workspaceDir.relativize(path)
                    },
                    jobValue.exitCode,
                  ),
                ),
              )
        }
    }
    job <- cached match
    case Some(job) => job.pure
    case _ =>
      val cmdlineString = desc.cmdline.mkString(" ")
      for
        _ <- ConsoleLogger[F].stdout(cmdlineString)
        _ <- Logger[F].info(s"starting job: $cmdlineString")
        envVars = desc.envVars.fold(null)(_.toSeq.map((k, v) => s"$k=$v").sorted.toArray)
        process <- Sync[F].blocking {
          java
            .lang
            .Runtime
            .getRuntime()
            .exec(
              cmdline,
              envVars,
              directory.toFile,
            )
        }
        // TODO: handle different encoding in InputStreamReader?
        stdPair <- Dispatcher[F].use { dispatcher =>
          (
            Sync[F].blocking {
              val lines = new BufferedReader(new InputStreamReader(process.getInputStream)).lines()
              val builder = new StringBuilder
              lines.forEach { line =>
                dispatcher.unsafeRunSync(ConsoleLogger[F].stdout(line))
                builder ++= line
                builder ++= ConsoleLogger.lineSeparator
              }
              builder.toString
            },
            Sync[F].blocking {
              val lines = new BufferedReader(new InputStreamReader(process.getErrorStream)).lines()
              val builder = new StringBuilder
              lines.forEach { line =>
                dispatcher.unsafeRunSync(ConsoleLogger[F].stderr(line))
                builder ++= line
                builder ++= ConsoleLogger.lineSeparator
              }
              builder.toString
            },
          ).parTupled
        }
        (stdout, stderr) = stdPair
        exitCode <- Sync[F].blocking(process.waitFor())
        _ <- Logger[F].info(s"finished job: $cmdlineString")
        missingFiles <- Sync[F].delay(outputPaths.filterNot(_.exists))
        job <-
          if exitCode != 0 && desc.fail.getOrElse(true) then
            ctx.error(src, s"$stderr\nnonzero exit code returned from job: $exitCode")
          else if missingFiles.nonEmpty then
            ctx.error(
              src,
              s"job did not produce expected output files: ${missingFiles.mkString(", ")}",
            )
          else Job(stdout, stderr, outputPaths, exitCode).pure
        outputPathHashes <- outputPaths.traverse(_.md5Hash(charset))
        jobValue = JobValue(outputPaths.zip(outputPathHashes), job.stdout, job.stderr, job.exitCode)
        _ <- Logger[F].info(s"inserting $jobValue into cache")
        _ <- cache.insert(jobKey, jobValue)
      yield job
  yield job
