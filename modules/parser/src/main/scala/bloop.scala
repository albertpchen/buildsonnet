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

import ch.epfl.scala.{bsp4j => bsp}

trait BloopServer extends bsp.BuildServer
  with bsp.ScalaBuildServer
  with bsp.JvmBuildServer

extension[T](promise: Promise[T])
  def cancel(): Unit =
    promise.tryFailure(new java.util.concurrent.CancellationException())

final class BuildsonnetBuildClient(
  workspace: java.nio.file.Path,
  logger: Logger
) extends bsp.BuildClient:
  val compilations = collection.concurrent.TrieMap.empty[String, Promise[Unit]]
  def cancel(): Unit =
    for
      key <- compilations.keysIterator
      compilation <- compilations.remove(key)
    do
      compilation.cancel()

  override def onBuildShowMessage(params: bsp.ShowMessageParams): Unit =
    logger.info(params.getMessage)

  override def onBuildLogMessage(params: bsp.LogMessageParams): Unit =
    logger.info(s"LOG: ${params.getMessage}")

  override def onBuildTaskStart(params: bsp.TaskStartParams): Unit =
    val isNoOp = params.getMessage.startsWith("Start no-op compilation")
    params.getDataKind match {
      case bsp.TaskDataKind.COMPILE_TASK =>
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

  override def onBuildTaskProgress(params: bsp.TaskProgressParams): Unit =
    if !(params.getMessage eq null) then
      logger.info(params.getMessage)

  override def onBuildTaskFinish(params: bsp.TaskFinishParams): Unit =
    params.getDataKind match {
      case bsp.TaskDataKind.COMPILE_REPORT =>
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

  override def onBuildPublishDiagnostics(params: bsp.PublishDiagnosticsParams): Unit =
    import scala.jdk.CollectionConverters.given
    import Logger.prefixLines
    val file = workspace.relativize(java.nio.file.Paths.get(new java.net.URI(params.getTextDocument.getUri)))
    params.getDiagnostics().asScala.foreach { diagnostic =>
      val startLine = diagnostic.getRange.getStart.getLine + 1
      val startCol = diagnostic.getRange.getStart.getCharacter + 1
      val header = s"${Console.UNDERLINED}$file${Console.RESET}:$startLine:$startCol"
      val logFn =
        diagnostic.getSeverity match
        case bsp.DiagnosticSeverity.ERROR => logger.error
        case bsp.DiagnosticSeverity.WARNING => logger.warn
        case _ => logger.info
      logFn(header)
      diagnostic.getMessage.prefixLines("  ").foreach(logFn)
      logFn("")
    }

  override def onBuildTargetDidChange(params: bsp.DidChangeBuildTarget): Unit = ???

object BuildsonnetBuildClient:
  def initializeBuildParams(workspaceURI: String): bsp.InitializeBuildParams = new bsp.InitializeBuildParams(
    "buildsonnet", // name of this client
    "0.0.1", // version of this client
    "1.0.0", // BSP version
    workspaceURI,
    new bsp.BuildClientCapabilities(java.util.Collections.singletonList("scala"))
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
  def compile(targetId: String): Future[bsp.CompileResult]
  def jvmRunEnvironment(targetId: String): Future[Either[bsp.StatusCode.ERROR.type | bsp.StatusCode.CANCELLED.type, bsp.JvmRunEnvironmentResult]]

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
      }

      def compile(targetId: String): Future[bsp.CompileResult] =
        val params = new bsp.CompileParams(
          java.util.Collections.singletonList(new bsp.BuildTargetIdentifier(s"file://$workspace/?id=$targetId"))
        )
        server.buildTargetCompile(params).asScala

      def jvmRunEnvironment(targetId: String): Future[Either[bsp.StatusCode.ERROR.type | bsp.StatusCode.CANCELLED.type, bsp.JvmRunEnvironmentResult]] =
        compile(targetId).flatMap { compileResult =>
          compileResult.getStatusCode match
          case bsp.StatusCode.OK =>
            val params = new bsp.JvmRunEnvironmentParams(
              java.util.Collections.singletonList(new bsp.BuildTargetIdentifier(s"file://$workspace/?id=$targetId"))
            )
            server.jvmRunEnvironment(params).asScala.map(Right(_))
          case status @ (bsp.StatusCode.ERROR | bsp.StatusCode.CANCELLED) =>
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
