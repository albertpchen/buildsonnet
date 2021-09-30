package root

import slick.jdbc.JdbcBackend.Database
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

case class JobDescription(
  cmdline: Seq[String],
  envVars: Option[Map[String, String]],
  outputFiles: Option[Seq[String]],
  inputFiles: Seq[EvaluatedJValue.JPath],
  stdin: Option[String],
  directory: Option[String],
) derives JDecoder

private[root] enum JobOutput:
  case OutputFileMissing(file: String)
  case AbsoluteOutputFile(file: String)
  case Success(file: String)

final class JobRunner private ():
  def run(
    ctx: EvaluationContext,
    src: Source,
    desc: JobDescription,
  ): Future[EvaluatedJValue.JJob] =
    given ExecutionContext = ctx.executionContext
    Future {
      val builder = new java.lang.ProcessBuilder(desc.cmdline: _*)
      val env = builder.environment()
      env.clear()
      desc.envVars.foreach(_.foreach(env.put(_, _)))
      desc.directory.foreach { directory =>
        if directory.startsWith("/") then ctx.error(src, s"job directory may not be an absolute path, got $directory")
      }
      val directory = builder.directory(desc.directory.fold(ctx.workspaceDir)(ctx.workspaceDir.resolve).normalize().toFile)
      val process = builder.start()
      val exitCode = process.waitFor()
      val stdOut = process
      val stdout = scala.io.Source.fromInputStream(process.getInputStream).mkString
      val stderr = scala.io.Source.fromInputStream(process.getErrorStream).mkString
      val missingFiles = collection.mutable.ArrayBuffer[String]()
      desc.outputFiles.foreach(_.foreach { output =>
        if ctx.workspaceDir.resolve(output).toFile.exists() then
          missingFiles += output
      })
      if missingFiles.nonEmpty then
        s"job did not produce expected output files\n  ${missingFiles.mkString("\n  ")}"
      val outputs: Seq[EvaluatedJValue.JPath] = desc.outputFiles.getOrElse(Seq.empty).map { output =>
        EvaluatedJValue.JPath(src, output)
      }
      EvaluatedJValue.JJob(src, desc, stdout, stderr, outputs, exitCode)
    }

object JobRunner:
  def apply(): JobRunner = new JobRunner()
