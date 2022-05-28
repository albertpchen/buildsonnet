package buildsonnet.job

import buildsonnet.evaluator.EvaluationContext
import buildsonnet.job.syntax.*

import cats.effect.{Sync, Async, Resource}
import cats.effect.std.Semaphore
import cats.syntax.all.given

import doobie.{ConnectionIO, Transactor}
import doobie.implicits.given
import doobie.util.ExecutionContexts
import doobie.hikari.HikariTransactor

import java.nio.file.Path
import java.security.MessageDigest

import org.typelevel.log4cats.Logger

object DoobieJobCache:
  private case class JobRow(
    job_id: Int,
    cmdline: Array[Byte],
    env_vars: Array[Byte],
    directory: String,
    input_signature: Array[Byte],
    stdin: String,
    stdout: String,
    stderr: String,
    exit_code: Int
  )

  private val create =
    sql"""
      CREATE TABLE IF NOT EXISTS jobs(
        job_id           INTEGER    PRIMARY KEY AUTOINCREMENT,
        cmdline          BLOB       NOT NULL,
        env_vars         BLOB       NOT NULL,
        directory        TEXT       NOT NULL,
        stdin            TEXT       NOT NULL,
        input_signature  BINARY(16) NOT NULL,
        stdout           TEXT       NOT NULL,
        stderr           TEXT       NOT NULL,
        exit_code        INTEGER    NOT NULL
      );
      CREATE TABLE IF NOT EXISTS paths(
        path      TEXT,
        signature BINARY(16) NOT NULL,
        job_id    BINARY(16) NOT NULL,
        PRIMARY KEY (path, job_id)
      )
    """.update.run

  private def update(
    cmdline: Array[Byte],
    env_vars: Array[Byte],
    directory: String,
    stdin: String,
    input_signature: Array[Byte],
    output_signature: Array[Byte],
    stdout: String,
    stderr: String,
    exit_code: Int,
  ) =
    sql"""
      UPDATE jobs
      SET
       cmdline         = $cmdline,        
       env_vars        = $env_vars,       
       directory       = $directory,      
       stdin           = $stdin,          
       input_signature = $input_signature,
       stdout          = $stdout,         
       stderr          = $stderr,         
      WHERE
        cmdline         = $cmdline AND
        env_vars        = $env_vars AND
        directory       = $directory AND
        stdin           = $stdin AND
        input_signature = $input_signature
    """.update.run

  private def queryJobs(
    cmdline: Array[Byte],
    env_vars: Array[Byte],
    directory: String,
    stdin: String,
    input_signature: Array[Byte],
  ) =
    sql"""
      SELECT
        job_id,
        cmdline,
        env_vars,
        directory,
        input_signature,
        stdin,
        stdout,
        stderr,
        exit_code
      FROM jobs
      WHERE
        cmdline         = $cmdline AND
        env_vars        = $env_vars AND
        directory       = $directory AND
        stdin           = $stdin AND
        input_signature = $input_signature
    """.query[JobRow].option

  private def deleteJobs(job_id: Int) =
    sql"DELETE FROM jobs WHERE job_id = $job_id".update.run

  private def insertJobs(
    cmdline: Array[Byte],
    env_vars: Array[Byte],
    directory: String,
    input_signature: Array[Byte],
    stdin: String,
    stdout: String,
    stderr: String,
    exit_code: Int,
  ) =
    sql"""
      INSERT INTO jobs (
        cmdline,
        env_vars,
        directory,
        input_signature,
        stdin,
        stdout,
        stderr,
        exit_code
      ) VALUES (
        $cmdline,
        $env_vars,
        $directory,
        $input_signature,
        $stdin,
        $stdout,
        $stderr,
        $exit_code
      )
    """.update.withUniqueGeneratedKeys[Int]("job_id")

  private def queryPaths(job_id: Int) =
    sql"""
      SELECT path, signature
      FROM paths
      WHERE job_id = $job_id
    """.query[(String, Array[Byte])].to[List]

  private def deletePaths(job_id: Int) =
    sql"DELETE FROM paths WHERE job_id = $job_id".update.run

  private def insertPaths(job_id: Int, path: String, hash: Array[Byte]) =
    sql"INSERT INTO paths (job_id, path, hash) VALUES ($job_id, $path, $hash)".update.run

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

  private def byteArrayToStrings(bytes: Array[Byte]): Seq[String] =
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

  def apply[F[_]: Async: Logger](workspaceDir: Path): Resource[F, JobCache[F]] =
    for
      pool <- ExecutionContexts.fixedThreadPool[F](1)
      _ <- Resource.eval(Logger[F].info(s"initializing hikari transactor at jdbc:sqlite:${workspaceDir}/buildsonnet.db"))
      transactor <- HikariTransactor.newHikariTransactor[F](
        "org.sqlite.JDBC",
        s"jdbc:sqlite:${workspaceDir}/buildsonnet.db",
        "", // user
        "", // password
        pool,
      )
      _ <- Resource.eval(create.transact(transactor))
      lock <- Resource.eval(Semaphore(1))
    yield
      new JobCache[F] {
        private def getRow(key: JobKey): ConnectionIO[Option[JobRow]] =
          for
            input_signature <-
              key.inputFiles.foldLeft(Sync[ConnectionIO].delay(MessageDigest.getInstance("MD5"))) {
                case (md5, (input, contentHash)) =>
                  for
                    md5 <- md5
                    nameHash = input.toString.md5Hash(charset)
                    _ <- Sync[ConnectionIO].delay {
                      md5.digest(nameHash)
                      md5.digest(contentHash)
                    }
                  yield
                    md5
              }
            jobRowOpt <- queryJobs(
              stringsToByteArray(key.cmdline),
              stringsToByteArray(key.envVars.flatMap((a, b) => Seq(a, b))),
              key.directory.toString,
              key.stdin,
              input_signature.digest,
            )
          yield
            jobRowOpt

        def get(key: JobKey): F[Option[JobValue]] =
          Resource.make(lock.acquire)(_ => lock.release).use { _ =>
            val transaction =
              for
                jobRowOpt <- getRow(key)
                outputPaths <- jobRowOpt.fold(List.empty.pure[ConnectionIO]) { jobRow =>
                  queryPaths(jobRow.job_id)
                }
              yield
                jobRowOpt.map { jobRow =>
                  JobValue(
                    outputPaths.map { (pathName, hash) =>
                      workspaceDir.resolve(pathName) -> hash
                    },
                    jobRow.stdout,
                    jobRow.stderr,
                    jobRow.exit_code,
                  )
                }
            transaction.transact(transactor)
          }

        def insert(key: JobKey, value: JobValue): F[Unit] =
          Resource.make(lock.acquire)(_ => lock.release).use { _ =>
            val transaction =
              for
                jobRowOpt <- getRow(key)
                _ <- jobRowOpt.fold(().pure[ConnectionIO]) { jobRow =>
                  for
                    _ <- deletePaths(jobRow.job_id)
                    _ <- deleteJobs(jobRow.job_id)
                    job_id <- insertJobs(
                      jobRow.cmdline,
                      jobRow.env_vars,
                      jobRow.directory,
                      jobRow.input_signature,
                      jobRow.stdin,
                      value.stdout,
                      value.stderr,
                      value.exitCode,
                    )
                    _ <- value.outputFiles.traverse { (path, hash) =>
                      insertPaths(job_id, path.toString, hash)
                    }
                  yield
                    ()
                }
              yield
                ()
            transaction.transact(transactor)
          }
      }
