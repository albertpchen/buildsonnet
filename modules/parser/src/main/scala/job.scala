package root

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

import slick.jdbc.JdbcBackend.Database
import slick.jdbc.SQLiteProfile.api
import slick.jdbc.SQLiteProfile.api.given

case class JobRow(
  cmdline: Array[Byte],
  envVars: Array[Byte],
  inputFiles: Array[Byte],
  stdin: String,
  directory: String,

  stdout: String,
  stderr: String,
  outputs: Array[Byte],
  exitCode: Int
)

private val mirror = summon[scala.deriving.Mirror.ProductOf[JobRow]]
val unapply = (a: JobRow) => Some(Tuple.fromProductTyped[JobRow](a))

private class JobTable(tag: api.Tag) extends api.Table[mirror.MirroredElemTypes](tag, "JOBS") {
  def cmdline = column[Array[Byte]]("cmdline")
  def envVars = column[Array[Byte]]("envVars")
  def inputFiles = column[Array[Byte]]("inputFiles")
  def stdin = column[String]("stdin")
  def directory = column[String]("directory")

  def pk = primaryKey("primary_key", (
    cmdline,
    envVars,
    inputFiles,
    stdin,
    directory,
  ))

  def stdout = column[String]("stdout")
  def stderr = column[String]("stderr")
  def outputs = column[Array[Byte]]("outputs")
  def exitCode = column[Int]("exitCode")
  def * = (
    cmdline,
    envVars,
    inputFiles,
    stdin,
    directory,

    stdout,
    stderr,
    outputs,
    exitCode
  )
}

sealed trait JobRunner:
  def run(
    ctx: EvaluationContext,
    src: Source,
    desc: JobDescription,
  ): Future[EvaluatedJValue.JJob]

object JobRunner:
  import java.nio.file.attribute.FileTime

  extension (path: java.nio.file.Path)
    def exists: Boolean = java.nio.file.Files.exists(path)
    def fileTime: FileTime =
      java.nio.file.Files.readAttributes(path, "lastModifiedTime")
        .get("lastModifiedTime")
        .asInstanceOf[FileTime]

  private given fileTimeOrdering: math.Ordering[FileTime] = new math.Ordering[FileTime]:
    def compare(x: FileTime, y: FileTime): Int =
      x.compareTo(y)

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

  private def byteArrayToString(bytes: Array[Byte]): Seq[String] =
    val buffer = new collection.mutable.ArrayBuffer[Byte]()
    val strings = new collection.mutable.ArrayBuffer[String]()
    var start = 0
    var end = 0
    for i <- 0 until bytes.size do
      end = i
      if bytes(i) == (0: Byte) then
        val stringBytes = Array.ofDim[Byte](end - start)
        for i <- start until end do stringBytes(i - start) = bytes(i)
        strings += new String(stringBytes, charset)
    strings.toSeq

  def apply()(using ExecutionContext): JobRunner = new JobRunner:
    lazy val database: Future[(api.TableQuery[JobTable], Database)] = {
      val jobTable = new api.TableQuery(new JobTable(_))
      val database = Database.forURL(
        s"jdbc:sqlite:${System.getProperty("user.dir")}/test.db",
        driver = "org.sqlite.JDBC",
      )
      database.run(jobTable.schema.createIfNotExists).map { _ => jobTable -> database }
    }
    def run(
      ctx: EvaluationContext,
      src: Source,
      desc: JobDescription,
    ): Future[EvaluatedJValue.JJob] =
      given ExecutionContext = ctx.executionContext
      database.flatMap { (jobTable, database) =>
        val builder = new java.lang.ProcessBuilder(desc.cmdline: _*)
        val env = builder.environment()
        env.clear()
        desc.envVars.foreach(_.foreach(env.put(_, _)))
        desc.directory.foreach { directory =>
          if directory.startsWith("/") then ctx.error(src, s"job directory may not be an absolute path, got $directory")
        }
        desc.outputFiles.foreach { output =>
          if output.startsWith("/") then ctx.error(src, s"job output file may not be an absolute path, got $output")
        }

        val outputPaths = desc.outputFiles.getOrElse(Seq.empty).map { file =>
          ctx.workspaceDir.resolve(file)
        }.distinct
        val isOutputStale =
          if desc.outputFiles.isEmpty then
            false
          else
            val inputTimesOpt = desc.inputFiles.foldLeft(Option(Seq.empty[FileTime])) {
              case (Some(tail), path) =>  if path.path.exists then Some(path.path.fileTime +: tail) else None
              case _ => None
            }
            val outputTimesOpt = outputPaths.foldLeft(Option(Seq.empty[FileTime])) {
              case (Some(tail), path) =>  if path.exists then Some(path.fileTime +: tail) else None
              case _ => None
            }
            inputTimesOpt.zip(outputTimesOpt).fold(false) { (inputTimes, outputTimes) =>
              import fileTimeOrdering.mkOrderingOps
              outputTimes.min < inputTimes.max
            }

        val directory = desc.directory.fold(ctx.workspaceDir)(ctx.workspaceDir.resolve).normalize()
        builder.directory(directory.toFile)

        val cached =
          if isOutputStale then
            Future(None)
          else
            val inputFilesBytes = stringsToByteArray(desc.inputFiles.map(_.path.toString))
            database.run(
              jobTable
                .filter(_.cmdline === stringsToByteArray(desc.cmdline))
                .filter(_.envVars === stringsToByteArray(desc.envVars.getOrElse(Map.empty).toSeq.sorted.flatMap((a, b) => Seq(a, b))))
                .filter(_.inputFiles === inputFilesBytes)
                .filter(_.stdin === desc.stdin.getOrElse(""))
                .filter(_.directory === directory.toString)
                .take(1)
                .result
            ).map(_.headOption)
        cached.map {
          case None =>
            val process = builder.start()
            val exitCode = process.waitFor()
            val stdOut = process
            val stdout = scala.io.Source.fromInputStream(process.getInputStream).mkString
            val stderr = scala.io.Source.fromInputStream(process.getErrorStream).mkString
            val missingFiles = collection.mutable.ArrayBuffer[String]()
            val outputFiles = desc.outputFiles.getOrElse(Seq.empty).distinct.map { output =>
              val path = ctx.workspaceDir.resolve(output).normalize
              if java.nio.file.Files.exists(path) then
                missingFiles += output
              path
            }
            if missingFiles.nonEmpty then
              s"job did not produce expected output files\n  ${missingFiles.mkString("\n  ")}"
            val outputs: Seq[EvaluatedJValue.JPath] = outputPaths.distinct.map(EvaluatedJValue.JPath(src, _))
            EvaluatedJValue.JJob(src, desc, stdout, stderr, outputs, exitCode)
          case Some(cached) =>
            val jobRow = mirror.fromProduct(cached)
            val outputs: Seq[EvaluatedJValue.JPath] = outputPaths.distinct.map(EvaluatedJValue.JPath(src, _))
            EvaluatedJValue.JJob(src, desc, jobRow.stdout, jobRow.stderr, outputs, jobRow.exitCode)
        }
      }
