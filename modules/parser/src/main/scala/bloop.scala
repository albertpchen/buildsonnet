package root

import bloop.launcher.Launcher
import bloop.launcher.LauncherMain

import ch.epfl.scala.bsp
import ch.epfl.scala.bsp.endpoints

import java.nio.channels.Channels
import java.nio.channels.Pipe
import java.util.concurrent.atomic.AtomicBoolean

import monix.execution.{ExecutionModel, Scheduler}
import monix.eval.Task

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import scala.concurrent.duration.{Duration, FiniteDuration}

trait Cancelable:
  def cancel(): Unit

object Cancelable:
  def apply(fn: () => Unit): Cancelable =
    new Cancelable {
      override def cancel(): Unit = fn()
    }
  val empty: Cancelable = Cancelable(() => ())


case class SocketConnection(
  serverName: String,
  output: java.io.OutputStream,
  input: java.io.InputStream,
  cancelables: List[Cancelable],
  finishedPromise: Promise[Unit]
)

def connectToLauncher(
  name: String,
  bloopVersion: String,
  bloopPort: Int,
  logStream: java.io.PrintStream,
)(using ec: Scheduler): Future[SocketConnection] =
  val launcherInOutPipe = Pipe.open()
  val launcherIn = Channels.newInputStream(launcherInOutPipe.source())
  val clientOut = Channels.newOutputStream(launcherInOutPipe.sink())

  val clientInOutPipe = Pipe.open()
  val clientIn = Channels.newInputStream(clientInOutPipe.source())
  val launcherOut = Channels.newOutputStream(clientInOutPipe.sink())

  val serverStarted = Promise[Unit]()
  val launcher = new LauncherMain(
    launcherIn,
    launcherOut,
    System.out,
    java.nio.charset.StandardCharsets.UTF_8,
    bloop.bloopgun.core.Shell.default,
    userNailgunHost = None,
    userNailgunPort = Some(bloopPort),
    serverStarted
  )

  val finished = Promise[Unit]()
  val job = ec.scheduleOnce(0, java.util.concurrent.TimeUnit.MILLISECONDS, new Runnable {
    override def run(): Unit = {
      launcher.runLauncher(
        bloopVersion,
        skipBspConnection = false,
        Nil
      )
      finished.success(())
    }
  })

  serverStarted.future.map { _ =>
    SocketConnection(
      name,
      clientOut,
      clientIn,
      List(
        Cancelable { () =>
          clientOut.flush()
          clientOut.close()
        },
        Cancelable(() => job.cancel())
      ),
      finished
    )
  }


final class BuildsonnetServices(
  val compilations: collection.concurrent.TrieMap[String, Promise[Unit]],
  val services: jsonrpc4s.Services,
):
  import BuildsonnetServices._
  def cancel(): Unit =
    for
      key <- compilations.keysIterator
      compilation <- compilations.remove(key)
    do
      compilation.cancel()


object BuildsonnetServices:
  extension[T](promise: Promise[T])
    def cancel(): Unit =
      promise.tryFailure(new java.util.concurrent.CancellationException())

  def apply(
    workspace: java.nio.file.Path,
    scribeLogger: scribe.Logger,
    logger: Logger,
  ): BuildsonnetServices =
    val compilations = collection.concurrent.TrieMap.empty[String, Promise[Unit]]
    val services = jsonrpc4s.Services.empty(scribeLogger)
      .notification(endpoints.Build.taskStart) { params =>
        params.dataKind match {
          case Some(bsp.TaskDataKind.CompileTask) =>
            val report = jsonrpc4s.RawJson.parseJsonTo[CustomBuildEndpoints.CompileReport](params.data.get)
            val id = report.toOption.get.target.uri.value
            compilations.remove(id).foreach(_.cancel())
            val isNoOp = params.message.getOrElse("").startsWith("Start no-op compilation")
            if !isNoOp then
              compilations(id) = Promise[Unit]()
              params.message.foreach(logger.info)
          case _ =>
            params.message.foreach(logger.info)
        }
      }
      .notification(endpoints.Build.taskProgress)(_ => {})
      .notification(endpoints.Build.taskFinish) { params =>
        params.dataKind match {
          case Some(bsp.TaskDataKind.CompileReport) =>
            val report = jsonrpc4s.RawJson.parseJsonTo[CustomBuildEndpoints.CompileReport](params.data.get)
            val id = report.toOption.get.target.uri.value
            compilations.get(id).foreach { promise =>
              params.message.foreach(logger.info)
              promise.success(())
            }
          case _ =>
            params.message.foreach(logger.info)
        }
      }
      .notification(endpoints.Build.showMessage)(params => logger.info(params.message))
      .notification(endpoints.Build.logMessage)(params => logger.info(params.message))
      .notification(endpoints.Build.publishDiagnostics) { params =>
        val file = workspace.relativize(params.textDocument.uri.toPath)
        params.diagnostics.foreach { diagnostic =>
          val startLine = diagnostic.range.start.line + 1
          val startCol = diagnostic.range.start.character + 1
          val header = s"${Console.UNDERLINED}$file${Console.RESET}:$startLine:$startCol"
          val logFn =
            diagnostic.severity match
            case Some(bsp.DiagnosticSeverity.Error) => logger.error(_: String)
            case Some(bsp.DiagnosticSeverity.Warning) => logger.warn(_: String)
            case _ => logger.info(_: String)
          logFn(header)
          import Logger.prefixLines
          diagnostic.message.prefixLines("  ").foreach(logFn)
          logFn("")
        }
      }
    new BuildsonnetServices(compilations, services)

def bsp4sClient(
  workspace: java.nio.file.Path,
  services: jsonrpc4s.Services,
  connection: SocketConnection,
  scribeLogger: scribe.Logger,
  scheduler: Scheduler,
): Task[jsonrpc4s.RpcClient] =
  val in = connection.input
  val out = connection.output
  implicit val client = jsonrpc4s.RpcClient.fromOutputStream(out, scribeLogger)
  val messages = jsonrpc4s.LowLevelMessage.fromInputStream(in, scribeLogger).map(jsonrpc4s.LowLevelMessage.toMsg)
  val lsServer = jsonrpc4s.RpcServer(messages, client, services, scheduler, scribeLogger)
  val runningClientServer = lsServer.startTask(Task.pure(())).runToFuture(using scheduler)
  for
    initializeResult <- endpoints.Build.initialize.request(
      bsp.InitializeBuildParams(
        displayName = "buildsonnet", // name of this client
        version = "0.0.1", // version of this client
        bspVersion = "2.0.0", // BSP version
        rootUri = bsp.Uri(workspace.toUri),
        capabilities = bsp.BuildClientCapabilities(List("scala")),
        data = None
      )
    )
    _ = endpoints.Build.initialized.notify(bsp.InitializedBuildParams())
  yield
    // TODO: handle error initializeResult
    client


sealed trait Bsp4sBloopServerConnection:
  def shutdown(): Task[Unit]
  def compile(targetId: String): Task[Either[String, bsp.CompileResult]]
  def jvmRunEnvironment(targetId: String): Task[Either[String, bsp.JvmRunEnvironmentResult]]
  def run(targetId: String, scalaMainClass: bsp.ScalaMainClass): Task[Either[String, bsp.RunResult]]
  def mainClasses(targetId: String): Task[Either[String, bsp.ScalaMainClassesResult]]

object Bsp4sBloopServerConnection:
  def std(
    workspace: java.nio.file.Path,
    logger: Logger,
    scribeLogger: scribe.Logger,
    bloopPort: Int,
    logStream: java.io.PrintStream,
  ): Bsp4sBloopServerConnection =
    new Bsp4sBloopServerConnection:
      private val launchedServer = Task(new AtomicBoolean(false)).memoize

      private val pair = Task.deferFutureAction { scheduler =>
        given Scheduler = scheduler
        val services = BuildsonnetServices(workspace, scribeLogger, logger)
        for
          connection <- connectToLauncher("buildsonnet", "1.4.9", bloopPort, logStream)
          client <- bsp4sClient(workspace, services.services, connection, scribeLogger, scheduler).runToFuture
        yield
          (connection, client, services)
      }.memoize
      private def connection = pair.map(_._1)
      private def services = pair.map(_._3)

      private val isShuttingDown = new AtomicBoolean(false)

      def shutdown(): Task[Unit] = launchedServer.flatMap { launchedServer =>
        if launchedServer.get() then
          Task.deferFutureAction { scheduler =>
            given Scheduler = scheduler
            pair.map { (connection, client, services) =>
              given jsonrpc4s.RpcClient = client
              try
                if (isShuttingDown.compareAndSet(false, true))
                  for
                    _ <- endpoints.Build.shutdown.request(bsp.Shutdown())
                    _ = endpoints.Build.exit.notify(bsp.Exit())
                  do
                    services.cancel()
                    connection.cancelables.foreach(_.cancel())
                    logStream.print("Shut down connection with bloop server.")
              catch
                case _: java.util.concurrent.TimeoutException =>
                  logger.error(s"timeout: bloop server during shutdown")
                case e: Throwable =>
                  logger.error(s"bloop server shutdown $e")
              logStream.close()
            }.runToFuture
          }
        else
          Task.now(logStream.close())
      }

      private def clientTask[T](task: jsonrpc4s.RpcClient ?=> Task[T]): Task[T] =
        pair.flatMap { (connection, client, services) =>
          task(using client)
        }

      def compile(targetId: String): Task[Either[String, bsp.CompileResult]] = clientTask {
        val params = bsp.CompileParams(
          List(bsp.BuildTargetIdentifier(bsp.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
            None,
            None,
        )
        endpoints.BuildTarget.compile.request(params).map {
          case jsonrpc4s.RpcSuccess(value, _) => Right(value)
          case jsonrpc4s.RpcFailure(methodName, underlying) => Left(jsonrpc4s.RpcFailure.toMsg(methodName, underlying))
        }
      }

      def jvmRunEnvironment(targetId: String): Task[Either[String, bsp.JvmRunEnvironmentResult]] = clientTask {
        compile(targetId).flatMap {
          case Right(compileResult) =>
            compileResult.statusCode match
            case bsp.StatusCode.Ok =>
              val params = bsp.JvmRunEnvironmentParams(
                List(bsp.BuildTargetIdentifier(bsp.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
                None
              )
              endpoints.BuildTarget.jvmRunEnvironment.request(params).map {
                case jsonrpc4s.RpcSuccess(value, _) => Right(value)
                case jsonrpc4s.RpcFailure(methodName, underlying) => Left(jsonrpc4s.RpcFailure.toMsg(methodName, underlying))
              }
            case bsp.StatusCode.Error =>
              Task.pure(Left(s"compilation for '$targetId' was cancelled"))
            case  bsp.StatusCode.Cancelled =>
              Task.pure(Left(s"compilation for '$targetId' failed"))
          case Left(msg) => Task.pure(Left(msg))
        }
      }

      def run(targetId: String, scalaMainClass: bsp.ScalaMainClass): Task[Either[String, bsp.RunResult]] = clientTask {
        compile(targetId).flatMap {
          case Right(compileResult) =>
            compileResult.statusCode match
            case bsp.StatusCode.Ok =>
              val params = bsp.RunParams(
                bsp.BuildTargetIdentifier(bsp.Uri(new java.net.URI(s"file://$workspace/?id=$targetId"))),
                originId = None,
                arguments = None,
                dataKind = Some(bsp.RunParamsDataKind.ScalaMainClass),
                data = Some(jsonrpc4s.RawJson.toJson(scalaMainClass)),
              )
              endpoints.BuildTarget.run.request(params).map {
                case jsonrpc4s.RpcSuccess(value, _) => Right(value)
                case jsonrpc4s.RpcFailure(methodName, underlying) => Left(jsonrpc4s.RpcFailure.toMsg(methodName, underlying))
              }
            case bsp.StatusCode.Error =>
              Task.pure(Left(s"compilation for '$targetId' was cancelled"))
            case  bsp.StatusCode.Cancelled =>
              Task.pure(Left(s"compilation for '$targetId' failed"))
          case Left(msg) => Task.pure(Left(msg))
        }
      }

      def mainClasses(targetId: String): Task[Either[String, bsp.ScalaMainClassesResult]] = clientTask {
        val params = bsp.ScalaMainClassesParams(
          List(bsp.BuildTargetIdentifier(bsp.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
          originId = None,
        )
        endpoints.BuildTarget.scalaMainClasses.request(params).map {
          case jsonrpc4s.RpcSuccess(value, _) => Right(value)
          case jsonrpc4s.RpcFailure(methodName, underlying) => Left(jsonrpc4s.RpcFailure.toMsg(methodName, underlying))
        }
      }
