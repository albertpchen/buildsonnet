package buildsonnet.bsp

import buildsonnet.logger.ConsoleLogger

import cats.Monad
import cats.effect.{Async, Resource, Sync}
import cats.effect.syntax.all.given
import cats.syntax.all.given

import ch.epfl.scala.{bsp => bsp4s}
import ch.epfl.scala.bsp.endpoints

import jsonrpc4cats.{
  LowLevelMessageWriter,
  RpcClient,
  RpcFailure,
  RpcResponse,
  RpcSuccess,
  Services,
}

import org.typelevel.log4cats.Logger

import scala.concurrent.duration.{FiniteDuration, given}

sealed trait BloopServer[F[_]]:
  def compile(targetId: String): F[RpcResponse[bsp4s.CompileResult]]
  def jvmRunEnvironment(targetId: String): F[RpcResponse[bsp4s.JvmRunEnvironmentResult]]
  def run(targetId: String, scalaMainClass: bsp4s.ScalaMainClass): F[RpcResponse[bsp4s.RunResult]]
  def mainClasses(targetId: String): F[RpcResponse[bsp4s.ScalaMainClassesResult]]
  def testClasses(targetId: String): F[RpcResponse[bsp4s.ScalaTestClassesResult]]
  def test(
    targetId: String,
    arguments: Option[List[String]],
    testParams: bsp4s.ScalaTestParams,
  ): F[RpcResponse[bsp4s.TestResult]]

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
      services <- Resource.eval(bspServices[F](workspace.toString))
      client <- RpcClient.setupBytes(
        fs2
          .io
          .readInputStream(
            socketConnection.input,
            chunkSize = 8192,
            closeAfterUse = true,
          ),
        services,
        maxConcurrentServiceWorkers,
      )
      _ <- Resource.make(
        LowLevelMessageWriter
          .toByteStream(client.messageStream)
          .through(fs2.io.writeOutputStream(socketConnection.output))
          .compile
          .drain
          .start,
      )(_.cancel)
      server <- Resource.make[F, Either[String, BloopServer[F]]] {
        val initParams = bsp4s.InitializeBuildParams(
          displayName = "buildsonnet", // name of this client
          version = "0.0.1", // version of this client
          bspVersion = "2.0.0", // BSP version
          rootUri = bsp4s.Uri(workspace.toUri),
          capabilities = bsp4s.BuildClientCapabilities(List("scala")),
          data = None,
        )
        def retry(backoff: FiniteDuration, numRetries: Int): F[Either[String, BloopServer[F]]] =
          Logger[F].info(s"Sending bsp initilize request") *>
            client.request(endpoints.Build.initialize, initParams).flatMap {
              case RpcFailure(_, error) =>
                if numRetries <= 0 then
                  Logger[F]
                    .error(s"failed to initialized bloop bsp (${error.getMessage}), giving up") *>
                    Left(error.getMessage).pure
                else
                  Logger[F].info(
                    s"failed to initialized bloop bsp ($error), retrying in $backoff seconds",
                  ) *>
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
    yield server

  private def makeServer[F[_]: Monad: Logger](workspace: java.nio.file.Path, client: RpcClient[F]) =
    new BloopServer[F] {
      def compile(targetId: String): F[RpcResponse[bsp4s.CompileResult]] =
        val params = bsp4s.CompileParams(
          List(
            bsp4s.BuildTargetIdentifier(
              bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")),
            ),
          ),
          None,
          None,
        )
        Logger[F].info("sending compile request") *>
          client.request(endpoints.BuildTarget.compile, params)

      def jvmRunEnvironment(targetId: String): F[RpcResponse[bsp4s.JvmRunEnvironmentResult]] =
        val params = bsp4s.JvmRunEnvironmentParams(
          List(
            bsp4s.BuildTargetIdentifier(
              bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")),
            ),
          ),
          None,
        )
        Logger[F].info("sending jvm run environment request") *>
          client.request(endpoints.BuildTarget.jvmRunEnvironment, params)

      def run(
        targetId: String,
        scalaMainClass: bsp4s.ScalaMainClass,
      ): F[RpcResponse[bsp4s.RunResult]] =
        val params = bsp4s.RunParams(
          bsp4s.BuildTargetIdentifier(
            bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")),
          ),
          originId = None,
          arguments = None,
          dataKind = Some(bsp4s.RunParamsDataKind.ScalaMainClass),
          data = Some(jsonrpc4cats.RawJson.toJson(scalaMainClass)),
        )
        Logger[F].info("sending run request") *>
          client.request(endpoints.BuildTarget.run, params)

      def mainClasses(targetId: String): F[RpcResponse[bsp4s.ScalaMainClassesResult]] =
        val params = bsp4s.ScalaMainClassesParams(
          List(
            bsp4s.BuildTargetIdentifier(
              bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")),
            ),
          ),
          originId = None,
        )
        Logger[F].info("sending main classes request") *>
          client.request(endpoints.BuildTarget.scalaMainClasses, params)

      def testClasses(targetId: String): F[RpcResponse[bsp4s.ScalaTestClassesResult]] =
        val params = bsp4s.ScalaTestClassesParams(
          List(
            bsp4s.BuildTargetIdentifier(
              bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")),
            ),
          ),
          originId = None,
        )
        Logger[F].info("sending test classes request") *>
          client.request(endpoints.BuildTarget.scalaTestClasses, params)

      def test(
        targetId: String,
        arguments: Option[List[String]],
        testParams: bsp4s.ScalaTestParams,
      ): F[RpcResponse[bsp4s.TestResult]] =
        val params = bsp4s.TestParams(
          List(
            bsp4s.BuildTargetIdentifier(
              bsp4s.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")),
            ),
          ),
          originId = None,
          arguments = arguments,
          dataKind = Some(bsp4s.TestParamsDataKind.ScalaTest),
          data = Some(jsonrpc4cats.RawJson.toJson(testParams)),
        )
        Logger[F].info("sending test request") *>
          client.request(endpoints.BuildTarget.test, params)
    }
