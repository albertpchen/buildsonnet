package buildsonnet.job

import buildsonnet.ast.{Source}
import buildsonnet.evaluator.{EvaluationContext}
import buildsonnet.logger.ConsoleLogger

import cats.effect.{Async, Sync}
import cats.effect.std.Dispatcher
import cats.syntax.all.given
import cats.Parallel

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, Paths}
import java.io.{BufferedReader, InputStreamReader}

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

case class JobRow(
  cmdline: Array[Byte],
  envVars: Array[Byte],
  inputFiles: Array[Byte],
  stdin: String,
  directory: String,

  stdout: String,
  stderr: String,
  exitCode: Int
)

case class Job(
  src: Source,
  desc: JobDescription,
  stdout: String,
  stderr: String,
  outputs: Seq[Path],
  exitCode: Int,
)

sealed trait JobRunner[F[_]]:
  def run(
    ctx: EvaluationContext[F],
    src: Source,
    desc: JobDescription,
  ): F[Job]

sealed trait JobCache[F[_]]:
  def get(desc: JobDescription): F[Option[Job]]
  def insert(job: Job): F[Unit]

object JobRunner:
  extension (path: Path)
    def exists: Boolean =
      Files.exists(path)

    def fileTime: FileTime =
      Files
        .readAttributes(path, "lastModifiedTime")
        .get("lastModifiedTime")
        .asInstanceOf[FileTime]

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

  private val charset = "utf-8"
  private def stringsToByteArray(strings: Seq[String]): Array[Byte] =
    if strings.isEmpty then Array.empty
    else
      val stream = new java.io.ByteArrayOutputStream()
      strings.foreach { string =>
        stream.write(string.getBytes(charset))
        stream.write(0)
      }
      stream.toByteArray()

  def apply[F[_]: Async: ConsoleLogger: Parallel](
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
          paths.find(path => !Files.exists(path)).fold(paths.pure) { path =>
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
        }
        isOutputStale <- Sync[F].delay {
          if desc.outputFiles.isEmpty then
            false
          else
            val outputTimesOpt = outputPaths.foldLeft(Option(Seq.empty[FileTime])) {
              case (Some(tail), path) => if path.exists then Some(path.fileTime +: tail) else None
              case _ => None
            }
            outputTimesOpt.fold(true) { outputTimes =>
              val inputTimes = inputPaths.map(_.fileTime)
              if outputTimes.isEmpty || inputTimes.isEmpty then
                false
              else
                outputTimes.min < inputTimes.max
            }
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
        cached <-
          if isOutputStale then None.pure
          else cache.get(desc)
        job <- cached match
          case Some(job) if !isOutputStale => job.pure
          case _ =>
            for
              _ <- ConsoleLogger[F].info(desc.cmdline.mkString(" "))
              process <- Sync[F].blocking {
                java.lang.Runtime.getRuntime().exec(
                  (cmd +: desc.cmdline.tail).toArray,
                  desc.envVars.fold(Seq.empty)(_.toSeq).map((k, v) => s"$k=$v").sorted.toArray,
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
                  val job = Job(src, desc, stdout, stderr, outputPaths, exitCode)
                  cache.insert(job).as(job)
            yield
              job
      yield
        cached.get
    }
