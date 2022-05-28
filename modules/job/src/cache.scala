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
    input_signature: String,
    stdin: String,
    stdout: String,
    stderr: String,
    exit_code: Int
  )

  private case class JobQueryParams(
    cmdline: Array[Byte],
    env_vars: Array[Byte],
    directory: String,
    stdin: String,
    input_signature: String,
  )

  private val createJobs =
    sql"""
      CREATE TABLE IF NOT EXISTS jobs(
        job_id           INTEGER    NOT NULL PRIMARY KEY AUTOINCREMENT,
        cmdline          BINARY     NOT NULL,
        env_vars         BINARY     NOT NULL,
        directory        TEXT       NOT NULL,
        stdin            TEXT       NOT NULL,
        input_signature  CHAR(32)   NOT NULL,
        stdout           TEXT       NOT NULL,
        stderr           TEXT       NOT NULL,
        exit_code        INTEGER    NOT NULL
      )
    """.update

  private val createPaths =
    sql"""
      CREATE TABLE IF NOT EXISTS paths(
        path      TEXT       NOT NULL,
        signature CHAR(32)   NOT NULL,
        job_id    INTEGER    NOT NULL,
        PRIMARY KEY (path, job_id)
      )
    """.update.run

  private def queryJobs(params: JobQueryParams) =
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
        cmdline         = ${params.cmdline} AND
        env_vars        = ${params.env_vars} AND
        directory       = ${params.directory} AND
        stdin           = ${params.stdin} AND
        input_signature = ${params.input_signature}
    """.query[JobRow]

  private def deleteJobs(job_id: Int) =
    sql"DELETE FROM jobs WHERE job_id = $job_id".update.run

  private def insertJobs(
    cmdline: Array[Byte],
    env_vars: Array[Byte],
    directory: String,
    input_signature: String,
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
    """.query[(String, String)].to[List]

  private def deletePaths(job_id: Int) =
    sql"DELETE FROM paths WHERE job_id = $job_id".update.run

  private def insertPaths(job_id: Int, path: String, signature: String) =
    sql"INSERT INTO paths (job_id, path, signature) VALUES ($job_id, $path, $signature)".update.run

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

  private def getQueryParams[F[_]: Sync](key: JobKey): F[JobQueryParams] =
    for
      input_signature <- Sync[F].delay {
        val md5 = MessageDigest.getInstance("MD5")
        key.inputFiles.foreach { (input, contentHash) =>
          val nameHash = input.toString.md5Hash(charset)
          md5.update(nameHash)
          md5.update(contentHash.md5Hash(charset))
          println(s"SLDFJLKJ: $contentHash")
        }
        val res = md5.digest.map("%02x".format(_)).mkString
        println(s"INPUT SINGATURE $res")
        res
      }
    yield
      JobQueryParams(
        stringsToByteArray(key.cmdline),
        stringsToByteArray(key.envVars.flatMap((a, b) => Seq(a, b))),
        key.directory.toString,
        key.stdin,
        input_signature,
      )

  def apply[F[_]: Async: Logger](workspaceDir: Path): Resource[F, JobCache[F]] =
    for
      pool <- ExecutionContexts.fixedThreadPool[F](1)
      _ <- Resource.eval(Logger[F].info(s"initializing sqlite at ${workspaceDir}/.buildsonnet.db"))
      transactor <- HikariTransactor.newHikariTransactor[F](
        "org.sqlite.JDBC",
        s"jdbc:sqlite:${workspaceDir}/.buildsonnet.db",
        "", // user
        "", // password
        pool,
      )
      _ <- Resource.eval((createJobs.run *> createPaths).transact(transactor))
      lock <- Resource.eval(Semaphore(1))
    yield
      new JobCache[F] {
        def getJobRow(params: JobQueryParams) =
          for
            jobRows <- queryJobs(params).to[List]
            jobRowOpt = if jobRows.isEmpty then None else Some(jobRows.maxBy(_.job_id))
            outputPaths <- jobRowOpt.fold(List.empty.pure[ConnectionIO]) { jobRow =>
              queryPaths(jobRow.job_id)
            }
            _ <- if jobRows.size > 1 then
              jobRows.traverse { jobRow =>
                if jobRow.job_id != jobRowOpt.get.job_id then
                  deleteJobs(jobRow.job_id).void
                else
                  ().pure[ConnectionIO]
              }
            else ().pure[ConnectionIO]
          yield
            jobRowOpt

        def get(key: JobKey): F[Option[JobValue]] =
          Resource.make(lock.acquire)(_ => lock.release).use { _ =>
            val transaction =
              for
                queryParams <- getQueryParams[ConnectionIO](key)
                jobRowOpt <- getJobRow(queryParams)
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
            // val y = transactor.yolo
            // import y._
            // getQueryParams[F](key).map(queryJobs).flatMap(_.check) *>
            transaction.transact(transactor)
          }

        def insert(key: JobKey, value: JobValue): F[Unit] =
          Resource.make(lock.acquire)(_ => lock.release).use { _ =>
            val transaction =
              for
                queryParams <- getQueryParams[ConnectionIO](key)
                jobRowOpt <- getJobRow(queryParams)
                _ <- jobRowOpt.fold(().pure[ConnectionIO]) { jobRow =>
                  for
                    _ <- deletePaths(jobRow.job_id)
                    _ <- deleteJobs(jobRow.job_id)
                  yield
                    ()
                }
                job_id <- insertJobs(
                  queryParams.cmdline,
                  queryParams.env_vars,
                  queryParams.directory,
                  queryParams.input_signature,
                  queryParams.stdin,
                  value.stdout,
                  value.stderr,
                  value.exitCode,
                )
                _ <- value.outputFiles.traverse { (path, hash) =>
                  insertPaths(job_id, path.toString, hash)
                }
              yield
                ()
            transaction.transact(transactor)
          }
      }
