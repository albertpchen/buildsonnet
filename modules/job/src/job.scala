package buildsonnet.job

import buildsonnet.ast.{Source}
import buildsonnet.evaluator.{EvaluationContext}
import buildsonnet.job.syntax.*
import buildsonnet.logger.ConsoleLogger

import cats.effect.{Async, Sync}
import cats.effect.std.Dispatcher
import cats.syntax.all.given
import cats.Parallel

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
)

private[job] enum JobOutput:
  case OutputFileMissing(file: String)
  case AbsoluteOutputFile(file: String)
  case Success(file: String)

case class JobKey(
  cmdline: Seq[String],
  envVars: Seq[(String, String)],
  inputFiles: Seq[(Path, Array[Byte])],
  stdin: String,
  directory: Path,
)

case class JobValue(
  outputFiles: Seq[(Path, Array[Byte])],
  stdout: String,
  stderr: String,
  exitCode: Int,
)

case class Job(
  src: Source,
  desc: JobDescription,
  stdout: String,
  stderr: String,
  outputs: Seq[Path],
  exitCode: Int,
)

trait JobRunner[F[_]]:
  def run(
    ctx: EvaluationContext[F],
    src: Source,
    desc: JobDescription,
  ): F[Job]

trait JobCache[F[_]]:
  def get(key: JobKey): F[Option[JobValue]]
  def insert(key: JobKey, value: JobValue): F[Unit]

object JobRunner:
  private given fileTimeOrdering: math.Ordering[FileTime] = new math.Ordering[FileTime]:
    def compare(x: FileTime, y: FileTime): Int =
      x.compareTo(y)

  import fileTimeOrdering.mkOrderingOps

  private def resolveCommand(cmd: String, path: Seq[String], dir: Path): Option[String] =
    if cmd.startsWith("/") then
      val path = Paths.get(cmd)
      return if Files.exists(path) then Some(path.toString) else None
    for p <- path do
      val path =
        if p.startsWith("/") then
          Paths.get(s"$p/$cmd")
        else
          dir.resolve(p).resolve(cmd)
      if Files.exists(path) then
        return Some(path.normalize().toString)
    return None

  def apply[F[_]: Async: ConsoleLogger: Parallel: Logger](
    ctx: EvaluationContext[F],
    cache: JobCache[F],
  ): JobRunner[F] =
    new JobRunner[F] {
      def run(
        ctx: EvaluationContext[F],
        src: Source,
        desc: JobDescription,
      ): F[Job] = for
        _ <-
          if desc.directory.exists(_.startsWith("/")) then
            ctx.error(src, s"job directory may not be an absolute path, got ${desc.directory.get}")
          else
            ().pure
        _ <-
          desc.outputFiles.find(_.startsWith("/")).fold(().pure) { output =>
            ctx.error(src, s"job output file may not be an absolute path, got $output")
          }
        _ <-
          desc.inputFiles.find(_.startsWith("/")).fold(().pure) { output =>
            ctx.error(src, s"job output file may not be an absolute path, got $output")
          }
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
          resolveCommand(
            desc.cmdline.head,
            desc.envVars.fold(Seq.empty)(_.get("PATH").fold(Seq.empty)(_.split(":").toSeq)) :+ ".",
            directory
          ).fold(ctx.error(src, s"could not resolve command \"${desc.cmdline.head}\""))(_.pure)
        }
        cmdline = (cmd +: desc.cmdline.tail).toArray
        inputPathHashes <- inputPaths.traverse(_.md5Hash)
        jobKey = JobKey(
          cmdline = cmdline,
          envVars = desc.envVars.fold(Seq.empty)(_.toList.sorted),
          inputFiles = inputPaths.zip(inputPathHashes),
          stdin = desc.stdin.getOrElse(""),
          directory = directory.relativize(ctx.workspaceDir),
        )
        cached <- cache.get(jobKey).flatMap {
          case None => None.pure
          case Some(jobValue) => Sync[F].delay {
            val paths = jobValue.outputFiles.map { (pathName, hash) =>
              ctx.workspaceDir.resolve(pathName) -> hash
            }
            val expectedOutputPaths = outputPaths.toSet
            if paths.exists { (path, hash) =>
              !path.exists || !expectedOutputPaths.contains(path) || path.md5Hash != hash
            } then
              None
            else
              Some(Job(
                src,
                desc,
                jobValue.stdout,
                jobValue.stderr,
                paths.map { (path, _) =>
                  ctx.workspaceDir.relativize(path)
                },
                jobValue.exitCode,
              ))
          }
        }
        job <- cached match
          case Some(job) => job.pure
          case _ =>
            for
              _ <- ConsoleLogger[F].info(desc.cmdline.mkString(" "))
              envVars = desc.envVars.fold(Seq.empty)(_.toSeq).map((k, v) => s"$k=$v").sorted.toArray
              process <- Sync[F].blocking {
                java.lang.Runtime.getRuntime().exec(
                  cmdline,
                  envVars,
                  directory.toFile
                )
              }
              // TODO: handle different encoding in InputStreamReader?
              stdPair <- Dispatcher[F].use { dispatcher =>
                (
                  Sync[F].blocking {
                    val lines = new BufferedReader(new InputStreamReader(process.getInputStream)).lines()
                    val builder = new StringBuilder
                    lines.forEach { line =>
                      dispatcher.unsafeRunTimed(ConsoleLogger[F].info(line), Duration.Inf)
                      builder ++= line
                      builder ++= ConsoleLogger.lineSeparator
                    }
                    builder.toString
                  },
                  Sync[F].blocking {
                    val lines = new BufferedReader(new InputStreamReader(process.getErrorStream)).lines()
                    val builder = new StringBuilder
                    lines.forEach { line =>
                      dispatcher.unsafeRunTimed(ConsoleLogger[F].info(line), Duration.Inf)
                      builder ++= line
                      builder ++= ConsoleLogger.lineSeparator
                    }
                    builder.toString
                  }
                ).parTupled
              }
              (stdout, stderr) = stdPair
              exitCode <- Sync[F].blocking(process.waitFor())
              missingFiles <- Sync[F].delay(outputPaths.filterNot(_.exists))
              job <-
                if exitCode != 0 then
                  ctx.error(src, s"$stderr\nnonzero exit code returned from job: $exitCode")
                else if missingFiles.nonEmpty then
                  ctx.error(src, s"job did not produce expected output files: ${missingFiles.mkString(", ")}")
                else
                  Job(src, desc, stdout, stderr, outputPaths, exitCode).pure
              outputPathHashes <- outputPaths.traverse(_.md5Hash)
              _ <- cache.insert(
                jobKey,
                JobValue(outputPaths.zip(outputPathHashes), job.stdout, job.stderr, job.exitCode),
              )
            yield
              job
      yield
        cached.get
    }
