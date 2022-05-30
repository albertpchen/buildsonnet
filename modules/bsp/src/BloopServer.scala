package buildsonnet.bsp

import buildsonnet.logger.ConsoleLogger

import cats.Monad
import cats.effect.{Async, Resource, Sync}
import cats.effect.syntax.all.given
import cats.syntax.all.given

import ch.epfl.scala.{bsp => bsp4s}
import ch.epfl.scala.bsp.endpoints

import jsonrpc4cats.{LowLevelMessageWriter, RpcClient, RpcFailure, RpcSuccess, Services}

import org.typelevel.log4cats.Logger

import scala.concurrent.duration.{FiniteDuration, given}


sealed trait BloopServer[F[_]]:
  def compile(targetId: String): F[Either[String, bsp4s.CompileResult]]
  def jvmRunEnvironment(targetId: String): F[Either[String, bsp4s.JvmRunEnvironmentResult]]
  def run(targetId: String, scalaMainClass: bsp4s.ScalaMainClass): F[Either[String, bsp4s.RunResult]]
  def mainClasses(targetId: String): F[Either[String, bsp4s.ScalaMainClassesResult]]
  // def test(targetId: String): F[Either[String, bsp4s.TestResult]]

object BloopServer:
  /** Initializes a bloop build server
    *
    * A shutdown request is sent to the server when the resource finalizes
    */
  def apply[F[_]: Async: Logger: ConsoleLogger](
    workspace: java.nio.file.Path,
    socketConnection: SocketConnection[F],
    maxConcurrentServiceWorkers: Int,
  ): Resource[F, Either[String, BloopServer[F]]] =
    for
      client <- RpcClient.setupBytes(
        fs2.io.readInputStream(
          socketConnection.input,
          chunkSize = 8192,
          closeAfterUse = true,
        ),
        bspServices[F](workspace.toString),
        maxConcurrentServiceWorkers,
      )
      _ <- Resource.make(
        LowLevelMessageWriter
          .toByteStream(client.messageStream)
          .through(fs2.io.writeOutputStream(socketConnection.output))
          .compile
          .drain
          .start
      )(_.cancel)
      server <- Resource.make[F, Either[String, BloopServer[F]]] {
        val initParams = bsp4s.InitializeBuildParams(
          displayName = "buildsonnet", // name of this client
          version = "0.0.1", // version of this client
          bspVersion = "2.0.0", // BSP version
          rootUri = bsp4s.Uri(workspace.toUri),
          capabilities = bsp4s.BuildClientCapabilities(List("scala")),
          data = None
        )
        def retry(backoff: FiniteDuration, numRetries: Int): F[Either[String, BloopServer[F]]] =
          Logger[F].info(s"Sending bsp initilize request") *>
          client.request(endpoints.Build.initialize, initParams).flatMap {
            case RpcFailure(_, error) =>
              if numRetries <= 0 then
                Logger[F].error(s"failed to initialized bloop bsp (${error.getMessage}), giving up") *>
                  Left(error.getMessage).pure
              else
                Logger[F].info(s"failed to initialized bloop bsp ($error), retrying in $backoff seconds") *>
                  Async[F].sleep(backoff) *> retry(backoff * 2, numRetries - 1)
            case RpcSuccess(_, _) =>
              Logger[F].info(s"successfully initialized bloop bsp") *>
                client.notify(endpoints.Build.initialized, bsp4s.InitializedBuildParams()) *>
                Right(makeServer(workspace, client)).pure
          }
        Logger[F].info(s"Attempting to connect to bloop build server") *>
          retry(1.seconds, 5)
      } {
        case Left(_) => ().pure // no need to shutdown if we never initialized
        case Right(_) => // TODO: should we not shutdown?
          for
            _ <- client.request(endpoints.Build.shutdown, bsp4s.Shutdown())
            _ <- client.notify(endpoints.Build.exit, bsp4s.Exit())
            _ <- Logger[F].info(s"Shut down connection with bloop server")
          yield ()
      }
    yield
      server

  private def makeServer[F[_]: Monad: Logger](workspace: java.nio.file.Path, client: RpcClient[F]) =
    new BloopServer[F] {
      def compile(targetId: String): F[Either[String, bsp4s.CompileResult]] =
        val params = bsp4s.CompileParams(
          List(bsp4s.BuildTargetIdentifier(bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
            None,
            None,
        )
        for
          _ <- Logger[F].info("sending compile request")
          response <- client.request(endpoints.BuildTarget.compile, params)
        yield
          response match
            case RpcSuccess(value, _) => Right(value)
            case RpcFailure(methodName, underlying) => Left(RpcFailure.toMsg(methodName, underlying))

      def jvmRunEnvironment(targetId: String): F[Either[String, bsp4s.JvmRunEnvironmentResult]] =
        compile(targetId).flatMap {
          case Left(msg) => Left(msg).pure
          case Right(compileResult) =>
            compileResult.statusCode match
              case bsp4s.StatusCode.Ok =>
                val params = bsp4s.JvmRunEnvironmentParams(
                  List(bsp4s.BuildTargetIdentifier(bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
                  None
                )
                client.request(endpoints.BuildTarget.jvmRunEnvironment, params).map {
                  case RpcSuccess(value, _) => Right(value)
                  case RpcFailure(methodName, underlying) => Left(RpcFailure.toMsg(methodName, underlying))
                }
              case bsp4s.StatusCode.Error =>
                Left(s"compilation for '$targetId' was cancelled").pure
              case  bsp4s.StatusCode.Cancelled =>
                Left(s"compilation for '$targetId' failed").pure
        }

      def run(targetId: String, scalaMainClass: bsp4s.ScalaMainClass): F[Either[String, bsp4s.RunResult]] =
        compile(targetId).flatMap {
          case Left(msg) => Left(msg).pure
          case Right(compileResult) =>
            compileResult.statusCode match
              case bsp4s.StatusCode.Ok =>
                val params = bsp4s.RunParams(
                  bsp4s.BuildTargetIdentifier(
                    bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId"))),
                  originId = None,
                  arguments = None,
                  dataKind = Some(bsp4s.RunParamsDataKind.ScalaMainClass),
                  data = Some(jsonrpc4cats.RawJson.toJson(scalaMainClass)),
                )
                client.request(endpoints.BuildTarget.run, params).map {
                  case RpcSuccess(value, _) => Right(value)
                  case RpcFailure(methodName, underlying) => Left(RpcFailure.toMsg(methodName, underlying))
                }
              case bsp4s.StatusCode.Error =>
                Left(s"compilation for '$targetId' was cancelled").pure
              case  bsp4s.StatusCode.Cancelled =>
                Left(s"compilation for '$targetId' failed").pure
        }

      def mainClasses(targetId: String): F[Either[String, bsp4s.ScalaMainClassesResult]] =
        val params = bsp4s.ScalaMainClassesParams(
          List(bsp4s.BuildTargetIdentifier(bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
          originId = None,
        )
        client.request(endpoints.BuildTarget.scalaMainClasses, params).map {
          case RpcSuccess(value, _) => Right(value)
          case RpcFailure(methodName, underlying) => Left(RpcFailure.toMsg(methodName, underlying))
        }
    }
