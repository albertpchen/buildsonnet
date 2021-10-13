package root

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import scala.concurrent.duration.Duration
import java.nio.channels.Channels
import java.nio.channels.Pipe
import bloop.launcher.Launcher

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

import java.nio.channels.Channels
import java.nio.channels.Pipe
import bloop.launcher.LauncherMain
def connectToLauncher(
  name: String,
  bloopVersion: String,
  bloopPort: Int,
  logStream: java.io.PrintStream,
)(using ec: ExecutionContextExecutorService): Future[SocketConnection] =
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
    logStream,
    java.nio.charset.StandardCharsets.UTF_8,
    bloop.bloopgun.core.Shell.default,
    userNailgunHost = None,
    userNailgunPort = Some(bloopPort),
    serverStarted
  )

  val finished = Promise[Unit]()
  val job = ec.submit(new Runnable {
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
        Cancelable(() => job.cancel(true))
      ),
      finished
    )
  }

import ch.epfl.scala.bsp4j

trait BloopServer extends bsp4j.BuildServer
  with bsp4j.ScalaBuildServer
  with bsp4j.JvmBuildServer

extension[T](promise: Promise[T])
  def cancel(): Unit =
    promise.tryFailure(new java.util.concurrent.CancellationException())

final class BuildsonnetBuildClient(
  workspace: java.nio.file.Path,
  logger: Logger
) extends bsp4j.BuildClient:
  val compilations = collection.concurrent.TrieMap.empty[String, Promise[Unit]]
  def cancel(): Unit =
    for
      key <- compilations.keysIterator
      compilation <- compilations.remove(key)
    do
      compilation.cancel()

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    logger.info(params.getMessage)

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    logger.info(s"LOG: ${params.getMessage}")

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    val isNoOp = params.getMessage.startsWith("Start no-op compilation")
    params.getDataKind match {
      case bsp4j.TaskDataKind.COMPILE_TASK =>
        val id =
          params
            .getData
            .asInstanceOf[com.google.gson.JsonObject]
            .getAsJsonObject("target")
            .getAsJsonPrimitive("uri")
            .getAsString
        compilations.remove(id).foreach(_.cancel())
        if !isNoOp then
          compilations(id) = Promise[Unit]()
      case _ =>
    }
    if !isNoOp then
      logger.info(params.getMessage)

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit =
    // if !(params.getMessage eq null) then
    //   logger.info(params.getMessage)
    ()

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    params.getDataKind match {
      case bsp4j.TaskDataKind.COMPILE_REPORT =>
        val id =
          params
            .getData
            .asInstanceOf[com.google.gson.JsonObject]
            .getAsJsonObject("target")
            .getAsJsonPrimitive("uri")
            .getAsString
        compilations.get(id).foreach { promise =>
          if !(params.getMessage eq null) then logger.info(params.getMessage)
          promise.success(())
        }
      case _ =>
        if !(params.getMessage eq null) then logger.info(params.getMessage)
    }

  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    import scala.jdk.CollectionConverters.given
    import Logger.prefixLines
    val file = workspace.relativize(java.nio.file.Paths.get(new java.net.URI(params.getTextDocument.getUri)))
    params.getDiagnostics().asScala.foreach { diagnostic =>
      val startLine = diagnostic.getRange.getStart.getLine + 1
      val startCol = diagnostic.getRange.getStart.getCharacter + 1
      val header = s"${Console.UNDERLINED}$file${Console.RESET}:$startLine:$startCol"
      val logFn =
        diagnostic.getSeverity match
        case bsp4j.DiagnosticSeverity.ERROR => logger.error
        case bsp4j.DiagnosticSeverity.WARNING => logger.warn
        case _ => logger.info
      logFn(header)
      diagnostic.getMessage.prefixLines("  ").foreach(logFn)
      logFn("")
    }

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit = ???

object BuildsonnetBuildClient:
  def initializeBuildParams(workspaceURI: String): bsp4j.InitializeBuildParams = new bsp4j.InitializeBuildParams(
    "buildsonnet", // name of this client
    "0.0.1", // version of this client
    "1.0.0", // BSP version
    workspaceURI,
    new bsp4j.BuildClientCapabilities(java.util.Collections.singletonList("scala"))
  )

import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.given
import scala.jdk.FutureConverters.given

sealed trait BloopServerConnection:
  def shutdown(): Future[Unit]
  def compile(targetId: String): Future[bsp4j.CompileResult]
  def jvmRunEnvironment(targetId: String): Future[Either[bsp4j.StatusCode.ERROR.type | bsp4j.StatusCode.CANCELLED.type, bsp4j.JvmRunEnvironmentResult]]

object BloopServerConnection:
  def std(
    workspace: java.nio.file.Path,
    logger: Logger,
    bloopPort: Int,
    logStream: java.io.PrintStream
  )(using ExecutionContextExecutorService): BloopServerConnection =
    new BloopServerConnection:
      private val launchedServer = new AtomicBoolean(false)

      lazy val pair = {
        val connection = Await.result(
          connectToLauncher("buildsonnet", "1.4.9", bloopPort, logStream), Duration.Inf)
        val (server, client, launcherListening) = Await.result(
          newServer(workspace, connection, logger), Duration.Inf)
        while !launchedServer.compareAndSet(false, true) do {}
        (server, client, connection, launcherListening)
      }

      private def server = pair._1
      private def client = pair._2
      private def connection = pair._3
      private def listening = pair._4
      private val isShuttingDown = new AtomicBoolean(false)

      def shutdown(): Future[Unit] = Future {
        if launchedServer.get() then
          try
            if (isShuttingDown.compareAndSet(false, true))
              // println("111")
              // println("222")
              server.buildShutdown().get()
              // println("333")
              server.onBuildExit()
              // println("444")
              listening.cancel(false)
              client.cancel() // cancel compilations
              // println("555")
              connection.finishedPromise.success(())
              // println("666")
              connection.cancelables.foreach(_.cancel())
              // println("777")
              logStream.print("Shut down connection with bloop server.")
          catch
            case _: TimeoutException =>
              logger.error(s"timeout: bloop server during shutdown")
            case e: Throwable =>
              logger.error(s"bloop server shutdown $e")
        logStream.close()
      }

      def compile(targetId: String): Future[bsp4j.CompileResult] =
        val params = new bsp4j.CompileParams(
          java.util.Collections.singletonList(new bsp4j.BuildTargetIdentifier(s"file://$workspace/?id=$targetId"))
        )
        server.buildTargetCompile(params).asScala

      def jvmRunEnvironment(targetId: String): Future[Either[bsp4j.StatusCode.ERROR.type | bsp4j.StatusCode.CANCELLED.type, bsp4j.JvmRunEnvironmentResult]] =
        compile(targetId).flatMap { compileResult =>
          compileResult.getStatusCode match
          case bsp4j.StatusCode.OK =>
            val params = new bsp4j.JvmRunEnvironmentParams(
              java.util.Collections.singletonList(new bsp4j.BuildTargetIdentifier(s"file://$workspace/?id=$targetId"))
            )
            server.jvmRunEnvironment(params).asScala.map(Right(_))
          case status @ (bsp4j.StatusCode.ERROR | bsp4j.StatusCode.CANCELLED) =>
            Future(Left(status))
        }

def newServer(
  workspace: java.nio.file.Path,
  connection: SocketConnection,
  logger: Logger,
)(using ec: ExecutionContextExecutorService): Future[(
  BloopServer,
  BuildsonnetBuildClient,
  java.util.concurrent.Future[Void],
)] =
  import java.util.concurrent.{Future => _, _}
  import org.eclipse.lsp4j.jsonrpc.Launcher

  val localClient = new BuildsonnetBuildClient(workspace, logger)

  val launcher = new Launcher.Builder[BloopServer]()
    .setOutput(connection.output)
    .setInput(connection.input)
    .setLocalService(localClient)
    .setExecutorService(ec)
    .setRemoteInterface(classOf[BloopServer])
    .create()
  val listening = launcher.startListening()
  val server = launcher.getRemoteProxy
  localClient.onConnectWithServer(server)

  import scala.jdk.FutureConverters.given
  server
    .buildInitialize(BuildsonnetBuildClient.initializeBuildParams(workspace.toUri().toString()))
    .asScala
    .map { _ =>
      server.onBuildInitialized()
      (server, localClient, listening)
    }

import ch.epfl.scala.bsp
import ch.epfl.scala.bsp.endpoints
import monix.execution.{ExecutionModel, Scheduler}
import scala.concurrent.duration.FiniteDuration
import monix.eval.Task

def bsp4sBuildsonnetBuildClient(
  workspace: java.nio.file.Path,
  scribeLogger: scribe.Logger,
  logger: Logger,
): jsonrpc4s.Services =
  val compilations = collection.concurrent.TrieMap.empty[String, Promise[Unit]]
  def cancel(): Unit =
    for
      key <- compilations.keysIterator
      compilation <- compilations.remove(key)
    do
      compilation.cancel()

  jsonrpc4s.Services.empty(scribeLogger)
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
    .notification(endpoints.Build.logMessage)(params => logger.info(s"LOG: ${params.message}"))
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

def bsp4sService(
  workspace: java.nio.file.Path,
  connection: SocketConnection,
  logger: Logger,
  scribeLogger: scribe.Logger,
  scheduler: Scheduler,
): Task[jsonrpc4s.RpcClient] =
  val in = connection.input
  val out = connection.output
  implicit val client = jsonrpc4s.RpcClient.fromOutputStream(out, scribeLogger)
  val messages = jsonrpc4s.LowLevelMessage.fromInputStream(in, scribeLogger).map(jsonrpc4s.LowLevelMessage.toMsg)
  val services = bsp4sBuildsonnetBuildClient(workspace, scribeLogger, logger)
  val lsServer = jsonrpc4s.RpcServer(messages, client, services, scheduler, scribeLogger)
  val runningClientServer = lsServer.startTask(Task.pure(())).runToFuture(using scheduler)
  val initializeServer = endpoints.Build.initialize.request(
    bsp.InitializeBuildParams(
      displayName = "buildsonnet", // name of this client
      version = "0.0.1", // version of this client
      bspVersion = "2.0.0", // BSP version
      rootUri = bsp.Uri(workspace.toUri),
      capabilities = bsp.BuildClientCapabilities(List("scala")),
      data = None
    )
  )
  for {
    // Delay the task to let the bloop server go live
    initializeResult <- initializeServer
    _ = endpoints.Build.initialized.notify(bsp.InitializedBuildParams())
  } yield {
    // TODO: handle error initializeResult
    client
  }


sealed trait Bsp4sBloopServerConnection:
  def shutdown(): Future[Unit]
  def compile(targetId: String): Future[Either[String, bsp.CompileResult]]
  def jvmRunEnvironment(targetId: String): Future[Either[String, bsp.JvmRunEnvironmentResult]]

object Bsp4sBloopServerConnection:
  def std(
    workspace: java.nio.file.Path,
    logger: Logger,
    scribeLogger: scribe.Logger,
    bloopPort: Int,
    logStream: java.io.PrintStream,
  )(using ExecutionContextExecutorService): Bsp4sBloopServerConnection =
    new Bsp4sBloopServerConnection:
      private val launchedServer = new AtomicBoolean(false)

      private given scheduler: Scheduler = Scheduler(implicitly[java.util.concurrent.ExecutorService])

      lazy val pair = {
        val connection = Await.result(
          connectToLauncher("buildsonnet", "1.4.9", bloopPort, logStream), Duration.Inf)
        val client =  Await.result(
          bsp4sService(workspace, connection, logger, scribeLogger, scheduler).runToFuture, Duration.Inf)
        (connection, client)
      }
      private def connection = pair._1
      given jsonrpc4s.RpcClient = pair._2

      private val isShuttingDown = new AtomicBoolean(false)

      def shutdown(): Future[Unit] = Future {
        if launchedServer.get() then
          try
            if (isShuttingDown.compareAndSet(false, true))
              for
                _ <- endpoints.Build.shutdown.request(bsp.Shutdown())
                _ = endpoints.Build.exit.notify(bsp.Exit())
              do
                // client.cancel() // TODO: cancel compilations
                connection.cancelables.foreach(_.cancel())
                logStream.print("Shut down connection with bloop server.")
          catch
            case _: TimeoutException =>
              logger.error(s"timeout: bloop server during shutdown")
            case e: Throwable =>
              logger.error(s"bloop server shutdown $e")
        logStream.close()
      }

      def compile(targetId: String): Future[Either[String, bsp.CompileResult]] =
        compileTask(targetId).runToFuture

      def compileTask(targetId: String): Task[Either[String, bsp.CompileResult]] =
        val params = bsp.CompileParams(
          List(bsp.BuildTargetIdentifier(bsp.Uri(new java.net.URI(s"file://$workspace/?id=$targetId")))),
            None,
            None,
        )
        endpoints.BuildTarget.compile.request(params).map {
          case jsonrpc4s.RpcSuccess(value, _) => Right(value)
          case jsonrpc4s.RpcFailure(methodName, underlying) => Left(jsonrpc4s.RpcFailure.toMsg(methodName, underlying))
        }

      def jvmRunEnvironment(targetId: String): Future[Either[String, bsp.JvmRunEnvironmentResult]] =
        jvmRunEnvironmentTask(targetId).runToFuture

      def jvmRunEnvironmentTask(targetId: String): Task[Either[String, bsp.JvmRunEnvironmentResult]] =
        compileTask(targetId).flatMap { compileResult =>
          compileResult match
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
