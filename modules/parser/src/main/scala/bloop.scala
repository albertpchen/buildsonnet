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
)(using ec: ExecutionContextExecutorService): Future[SocketConnection] =
  val launcherInOutPipe = Pipe.open()
  val launcherIn = Channels.newInputStream(launcherInOutPipe.source())
  val clientOut = Channels.newOutputStream(launcherInOutPipe.sink())

  val clientInOutPipe = Pipe.open()
  val clientIn = Channels.newInputStream(clientInOutPipe.source())
  val launcherOut = Channels.newOutputStream(clientInOutPipe.sink())

  val serverStarted = Promise[Unit]()
  val launcher =
    new LauncherMain(
      launcherIn,
      launcherOut,
      System.err,
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

import ch.epfl.scala.bsp4j.{
  BuildClient,
  BuildServer,
  BuildClientCapabilities,
  BuildTargetIdentifier,
  CompileParams,
  CompileResult,
  DiagnosticSeverity,
  InitializeBuildParams,
  ScalaBuildServer,
  ShowMessageParams,
  LogMessageParams,
  TaskDataKind,
  TaskStartParams,
  TaskProgressParams,
  TaskFinishParams,
  PublishDiagnosticsParams,
  DidChangeBuildTarget,
}

trait BloopServer extends BuildServer with ScalaBuildServer

extension[T](promise: Promise[T])
  def cancel(): Unit =
    promise.tryFailure(new java.util.concurrent.CancellationException())

final class BuildsonnetBuildClient(
  workspace: java.nio.file.Path,
  logger: Logger
) extends BuildClient:
  val compilations = collection.concurrent.TrieMap.empty[String, Promise[Unit]]
  def cancel(): Unit =
    for {
      key <- compilations.keysIterator
      compilation <- compilations.remove(key)
    } {
      compilation.cancel()
    }

  override def onBuildShowMessage(params: ShowMessageParams): Unit =
    logger.info(params.getMessage)

  override def onBuildLogMessage(params: LogMessageParams): Unit =
    logger.info(s"LOG: ${params.getMessage}")

  override def onBuildTaskStart(params: TaskStartParams): Unit =
    params.getDataKind match {
      case TaskDataKind.COMPILE_TASK =>
        val id =
          params
            .getData
            .asInstanceOf[com.google.gson.JsonObject]
            .getAsJsonObject("target")
            .getAsJsonPrimitive("uri")
            .getAsString
        compilations.remove(id).foreach(_.cancel())
        compilations(id) = Promise[Unit]()
      case _ =>
    }
    logger.info(params.getMessage)

  override def onBuildTaskProgress(params: TaskProgressParams): Unit =
    if !(params.getMessage eq null) then
      logger.info(params.getMessage)

  override def onBuildTaskFinish(params: TaskFinishParams): Unit =
    params.getDataKind match {
      case TaskDataKind.COMPILE_REPORT =>
        val id =
          params
            .getData
            .asInstanceOf[com.google.gson.JsonObject]
            .getAsJsonObject("target")
            .getAsJsonPrimitive("uri")
            .getAsString
        compilations.get(id).foreach(_.success(()))
      case _ =>
    }
    if !(params.getMessage eq null) then
      logger.info(params.getMessage)
      //println(params.getData.asInstanceOf[com.google.gson.JsonObject].getAsJsonPrimitive("clientDir"))

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit =
    import scala.jdk.CollectionConverters.given
    import Logger.prefixLines
    val file = workspace.relativize(java.nio.file.Paths.get(new java.net.URI(params.getTextDocument.getUri)))
    params.getDiagnostics().asScala.foreach { diagnostic =>
      val startLine = diagnostic.getRange.getStart.getLine + 1
      val startCol = diagnostic.getRange.getStart.getCharacter + 1
      val header = s"${Console.UNDERLINED}$file${Console.RESET}:$startLine:$startCol"
      val logFn =
        diagnostic.getSeverity match
        case DiagnosticSeverity.ERROR => logger.error
        case DiagnosticSeverity.WARNING => logger.warn
        case _ => logger.info
      logFn(header)
      diagnostic.getMessage.prefixLines("  ").foreach(logFn)
      logFn("")
    }

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ???

object BuildsonnetBuildClient:
  def initializeBuildParams(workspaceURI: String): InitializeBuildParams = new InitializeBuildParams(
    "buildsonnet", // name of this client
    "0.0.1", // version of this client
    "1.0.0", // BSP version
    workspaceURI,
    new BuildClientCapabilities(java.util.Collections.singletonList("scala"))
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
  def compile(targetId: String): Future[CompileResult]

object BloopServerConnection:
  def empty: BloopServerConnection = new BloopServerConnection:
    def shutdown(): Future[Unit] = ???
    def compile(targetId: String): Future[CompileResult] = ???

  def std(
    workspace: java.nio.file.Path,
    logger: Logger,
    bloopPort: Int,
  )(using ExecutionContextExecutorService): BloopServerConnection =
    new BloopServerConnection:
      private val launchedServer = new AtomicBoolean(false)

      lazy val pair = {
        val connection = Await.result(
          connectToLauncher("buildsonnet", "1.4.9", bloopPort), Duration.Inf)
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
          println("111")
          try {
            if (isShuttingDown.compareAndSet(false, true)) {
              client.cancel() // cancel compilations
              println("333")
              server.buildShutdown().get()
              println("444")
              server.onBuildExit()
              println("555")
              listening.cancel(false)
              connection.cancelables.foreach(_.cancel())
              connection.finishedPromise.success(())
              logger.info("Shut down connection with bloop server.")
              println("666")
              println("777")
              println("888")
            }
          } catch {
            case _: TimeoutException =>
              logger.error(s"timeout: bloop server during shutdown")
            case e: Throwable =>
              logger.error(s"bloop server shutdown $e")
          }
      }

      def compile(targetId: String): Future[CompileResult] =
        server.buildTargetCompile(
          new CompileParams(
            java.util.Collections.singletonList(new BuildTargetIdentifier(s"file://$workspace/?id=$targetId")))
        ).asScala

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
  val server = launcher.getRemoteProxy
  localClient.onConnectWithServer(server)
  val listening = launcher.startListening()
  import scala.jdk.FutureConverters.given
  val initializeResult = server.buildInitialize(
    BuildsonnetBuildClient.initializeBuildParams(workspace.toUri().toString()))
  import scala.jdk.CollectionConverters.given
  for
    _ <- initializeResult.asScala
    //workspaceBuildTargetsResult <- server.workspaceBuildTargets().asScala
  yield
    server.onBuildInitialized()
    (server, localClient, listening)
