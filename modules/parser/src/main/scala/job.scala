package root

import monix.eval.Task
import monix.execution.Scheduler

import slick.jdbc.JdbcBackend.Database
import slick.jdbc.SQLiteProfile.api
import slick.jdbc.SQLiteProfile.api.given

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

object JobRow:
  private val mirror = summon[scala.deriving.Mirror.ProductOf[JobRow]]
  type TupleType = mirror.MirroredElemTypes
  def fromTuple(tuple: TupleType): JobRow = mirror.fromProduct(tuple)

private class JobTable(tag: api.Tag) extends api.Table[JobRow.TupleType](tag, "JOBS") {
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
  def exitCode = column[Int]("exitCode")
  def * = (
    cmdline,
    envVars,
    inputFiles,
    stdin,
    directory,

    stdout,
    stderr,
    exitCode
  )
}

sealed trait JobRunner:
  def run(
    ctx: EvaluationContext,
    src: Source,
    desc: JobDescription,
  ): Task[EvaluatedJValue.JJob]

object JobRunner:
  import java.nio.file.attribute.FileTime

  def write(path: java.nio.file.Path, contents: String): java.nio.file.Path =
    if !java.nio.file.Files.exists(path) || !JobRunner.fileMatchesContents(path.toFile, contents) then
      java.nio.file.Files.write(path, contents.getBytes())
    path

  def fileMatchesContents(file: java.io.File, contents: String): Boolean =
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


  private given fileTimeOrdering: math.Ordering[FileTime] = new math.Ordering[FileTime]:
    def compare(x: FileTime, y: FileTime): Int =
      x.compareTo(y)

  import fileTimeOrdering.mkOrderingOps

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

  extension (ctx: EvaluationContext)
    def resolvePath(path: String): java.nio.file.Path =
      java.nio.file.Paths.get(path).normalize()

    def exists(path: java.nio.file.Path): Boolean =
      java.nio.file.Files.exists(ctx.workspaceDir.resolve(path))

    def fileTime(path: java.nio.file.Path): FileTime =
      java.nio.file.Files.readAttributes(ctx.workspaceDir.resolve(path), "lastModifiedTime")
        .get("lastModifiedTime")
        .asInstanceOf[FileTime]

    def fileHash(path: java.nio.file.Path): Task[String] = Task {
      import java.security.{MessageDigest, DigestInputStream}
      val buffer = Array.ofDim[Byte](8192)
      val md5 = MessageDigest.getInstance("MD5")

      val dis = new DigestInputStream(new java.io.FileInputStream(path.toFile), md5)
      try { while (dis.read(buffer) != -1) { } } finally { dis.close() }

      md5.digest.map("%02x".format(_)).mkString
    }

  def apply(): JobRunner = new JobRunner:
    def database(ctx: EvaluationContext): Task[(api.TableQuery[JobTable], Database)] =
      val jobTable = new api.TableQuery(new JobTable(_))
      val database = Database.forURL(
        s"jdbc:sqlite:${ctx.workspaceDir.toString}/buildsonnet.db",
        driver = "org.sqlite.JDBC",
      )
      Task.deferFutureAction(database.run(jobTable.schema.createIfNotExists).map { _ => jobTable -> database })

    private def resolveCommand(
      cmd: String,
      path: Seq[String],
      dir: java.nio.file.Path
    ): Option[String] =
      if cmd.startsWith("/") then
        val path = java.nio.file.Paths.get(cmd)
        return if java.nio.file.Files.exists(path) then Some(path.toString) else None
      for p <- path do
        val path =
          if p.startsWith("/") then
            java.nio.file.Paths.get(s"$p/$cmd")
          else
            dir.resolve(p).resolve(cmd)
        if java.nio.file.Files.exists(path) then
          return Some(path.normalize().toString)
      return None

    def run(
      ctx: EvaluationContext,
      src: Source,
      desc: JobDescription,
    ): Task[EvaluatedJValue.JJob] =
      database(ctx).flatMap { (jobTable, database) =>
        desc.directory.foreach { directory =>
          if directory.startsWith("/") then ctx.error(src, s"job directory may not be an absolute path, got $directory")
        }
        desc.outputFiles.foreach { output =>
          if output.startsWith("/") then ctx.error(src, s"job output file may not be an absolute path, got $output")
        }
        desc.inputFiles.foreach { input =>
          if !ctx.exists(input.path) then ctx.error(src, s"job input file does not exist, ${input.path.toString}")
        }
        if desc.cmdline.size <= 0 then
          ctx.error(src, s"job cmdline must be a non-empty list")

        val outputPaths = desc.outputFiles.getOrElse(Seq.empty).map(ctx.resolvePath(_)).distinct
        val isOutputStale =
          if desc.outputFiles.isEmpty then
            false
          else
            val outputTimesOpt = outputPaths.foldLeft(Option(Seq.empty[FileTime])) {
              case (Some(tail), path) =>  if ctx.exists(path) then Some(ctx.fileTime(path) +: tail) else None
              case _ => None
            }
            outputTimesOpt.fold(true) { outputTimes =>
              val inputTimes = desc.inputFiles.map(p => ctx.fileTime(p.path))
              if outputTimes.isEmpty || inputTimes.isEmpty then
                false
              else
                outputTimes.min < inputTimes.max
            }

        val directory = desc.directory.fold(ctx.workspaceDir)(ctx.resolvePath(_))
        val cmd = resolveCommand(
          desc.cmdline.head,
          desc.envVars.fold(Seq.empty)(_.get("PATH").fold(Seq.empty)(_.split(":").toSeq)) :+ ".",
          directory
        ).getOrElse(ctx.error(src, s"could not resolve command \"${desc.cmdline.head}\""))

        val cached =
          if isOutputStale then
            Task.now(None)
          else
            val inputFilesBytes = stringsToByteArray(desc.inputFiles.map(_.path.toString))
            Task.deferFutureAction(a => {
              given Scheduler = a
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
          })
        cached.flatMap {
          case Some(cached) if !isOutputStale =>
            val jobRow = JobRow.fromTuple(cached)
            val outputs: Seq[EvaluatedJValue.JPath] = outputPaths.distinct.map(EvaluatedJValue.JPath(src, _))
            Task.now(EvaluatedJValue.JJob(src, desc, jobRow.stdout, jobRow.stderr, outputs, jobRow.exitCode))
          case _ =>
            Task.deferFutureAction(s => {
              println(desc.cmdline.mkString(" ")) // print cmdline feedback before running
              val process =
                java.lang.Runtime.getRuntime().exec(
                  (cmd +: desc.cmdline.tail).toArray,
                  desc.envVars.fold(Seq.empty)(_.toSeq).map((k, v) => s"$k=$v").sorted.toArray,
                  directory.toFile
                )
              val stdoutObservable =
                monix.reactive.Observable.fromLinesReaderUnsafe(new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream), 2048))
              val printAndBuildConsumer: monix.reactive.Consumer[String, StringBuilder] =
              monix.reactive.Consumer.foldLeft(new StringBuilder) { (builder, line) =>
                println(line)
                builder ++= line
                builder ++= Logger.lineSeparator
                builder
              }
              given Scheduler = s
              val stdout =
                monix.reactive.Observable.fromLinesReaderUnsafe(new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream), 2048))
                  .consumeWith(printAndBuildConsumer)
                  .map(_.toString)
                  .runToFuture
              val stderr =
                monix.reactive.Observable.fromLinesReaderUnsafe(new java.io.BufferedReader(new java.io.InputStreamReader(process.getErrorStream), 2048))
                  .consumeWith(printAndBuildConsumer)
                  .map(_.toString)
                  .runToFuture
              val outputs: Seq[EvaluatedJValue.JPath] = outputPaths.distinct.map(EvaluatedJValue.JPath(src, _))
              // print(stdout)
              val exitCode = process.waitFor()
              val missingFiles = outputPaths.filterNot(ctx.exists)
              stdout.zip(stderr).flatMap { (stdout, stderr) =>
                if exitCode != 0 then
                  //System.err.println(stderr)
                  ctx.error(src, s"$stderr\nnonzero exit code returned from job: $exitCode")
                else if missingFiles.nonEmpty then
                  ctx.error(src, s"job did not produce expected output files: ${missingFiles.mkString(", ")}")
                else
                  database.run(jobTable.insertOrUpdate((
                    stringsToByteArray(desc.cmdline),
                    stringsToByteArray(desc.envVars.getOrElse(Map.empty).toSeq.sorted.flatMap((a, b) => Seq(a, b))),
                    stringsToByteArray(desc.inputFiles.map(_.path.toString)),
                    desc.stdin.getOrElse(""),
                    directory.toString,

                    stdout,
                    stderr,
                    exitCode,
                  ))).map { _ =>
                    EvaluatedJValue.JJob(src, desc, stdout, stderr, outputs, exitCode)
                  }
              }
            })
        }
      }
