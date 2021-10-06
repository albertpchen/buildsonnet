package root

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import java.nio.channels.Channels
import java.nio.channels.Pipe
import bloop.launcher.Launcher

trait Cancelable {
  def cancel(): Unit
}

object Cancelable {
  def apply(fn: () => Unit): Cancelable =
    new Cancelable {
      override def cancel(): Unit = fn()
    }
  val empty: Cancelable = Cancelable(() => ())
}

import scala.concurrent.{ExecutionContextExecutorService, Promise}
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
)(using ec: ExecutionContextExecutorService): Future[SocketConnection] = {
  val launcherInOutPipe = Pipe.open()
  val launcherIn = Channels.newInputStream(launcherInOutPipe.source())
  val clientOut = Channels.newOutputStream(launcherInOutPipe.sink())

  val clientInOutPipe = Pipe.open()
  val clientIn = Channels.newInputStream(clientInOutPipe.source())
  val launcherOut = Channels.newOutputStream(clientInOutPipe.sink())

  println("111")
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
  println("222")

  val finished = Promise[Unit]()
  val job = ec.submit(new Runnable {
    override def run(): Unit = {
      println("666")
      launcher.runLauncher(
        bloopVersion,
        skipBspConnection = false,
        Nil
      )
      println("777")
      finished.success(())
    }
  })
  println("333")

  serverStarted.future.map { _ =>
    println("444")
    SocketConnection(
      name,
      clientOut,
      clientIn,
      List(
        Cancelable { () =>
          println("555")
          clientOut.flush()
          clientOut.close()
        },
        Cancelable(() => job.cancel(true))
      ),
      finished
    )
  }
}

import ch.epfl.scala.bsp4j.{
  BuildClient,
  BuildServer,
  BuildClientCapabilities,
  BuildTargetIdentifier,
  CompileParams,
  CompileReport,
  InitializeBuildParams,
  ScalaBuildServer,
  ShowMessageParams,
  LogMessageParams,
  TaskStartParams,
  TaskProgressParams,
  TaskFinishParams,
  PublishDiagnosticsParams,
  DidChangeBuildTarget,
}

trait BloopServer extends BuildServer with ScalaBuildServer

final class BuildsonnetBuildClient extends BuildClient:
  override def onBuildShowMessage(params: ShowMessageParams): Unit =
    println(params.getMessage)

  override def onBuildLogMessage(params: LogMessageParams): Unit =
    println(s"LOG: ${params.getMessage}")

  override def onBuildTaskStart(params: TaskStartParams): Unit =
    println(params.getMessage)

  override def onBuildTaskProgress(params: TaskProgressParams): Unit =
    if !(params.getMessage eq null) then
      println(params.getMessage)

  override def onBuildTaskFinish(params: TaskFinishParams): Unit =
    if !(params.getMessage eq null) then
      println(params.getMessage)
      //println(params.getData.asInstanceOf[com.google.gson.JsonObject].getAsJsonPrimitive("clientDir"))

  override def onBuildPublishDiagnostics(params: PublishDiagnosticsParams): Unit =
    import scala.jdk.CollectionConverters.given
    params.getDiagnostics().asScala.map { diagnostic =>
      val startLine = diagnostic.getRange.getStart.getLine + 1
      val startCol = diagnostic.getRange.getStart.getCharacter + 1
      val endLine = diagnostic.getRange.getEnd.getLine + 1
      val endCol = diagnostic.getRange.getEnd.getCharacter + 1
      val sourceLoc =
        if startLine == (endLine) then
          if startCol == endCol then
            s"$startLine:$startCol"
          else
            s"$startLine:$startCol-$endCol"
        else
          s"[$startLine:$startCol]:[$endLine:$endCol]"
      s"""$startLine:$startCol:${params.getTextDocument.getUri}
         |  ${diagnostic.getMessage}
         |""".stripMargin
    }.foreach(println)

  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ???

object BuildsonnetBuildClient:
  def initializeBuildParams(workspaceURI: String): InitializeBuildParams = new InitializeBuildParams(
    "buildsonnet", // name of this client
    "0.0.1", // version of this client
    "1.0.0", // BSP version
    workspaceURI,
    new BuildClientCapabilities(java.util.Collections.singletonList("scala"))
  )

case class BloopServerConnection(
  server: BloopServer,
  connection: SocketConnection,
)

def client(
  workspace: java.nio.file.Path,
  connection: SocketConnection,
)(using ec: ExecutionContextExecutorService): Future[Unit] =
  import java.util.concurrent.{Future => _, _}
  import org.eclipse.lsp4j.jsonrpc.Launcher

  val localClient = new BuildsonnetBuildClient

  val launcher = new Launcher.Builder[BloopServer]()
    .setOutput(connection.output)
    .setInput(connection.input)
    .setLocalService(localClient)
    .setExecutorService(ec)
    .setRemoteInterface(classOf[BloopServer])
    .create()
  println("launch 111")
  val server = launcher.getRemoteProxy
  println("launch 222")
  localClient.onConnectWithServer(server)
  println("launch 333")
  val listening = launcher.startListening()
  println("launch 444")
  import scala.jdk.FutureConverters.given
  val initializeResult = server.buildInitialize(
    BuildsonnetBuildClient.initializeBuildParams(workspace.toUri().toString()))
  println("launch 555")
  import scala.jdk.CollectionConverters.given
  for
    initializeBuildResult <- initializeResult.asScala
    _ = println(s"init 111")
    _ = server.onBuildInitialized()
    _ = println("init 222")
    compileResult <- server.buildTargetCompile(
      new CompileParams(Seq(new BuildTargetIdentifier(s"file://$workspace/?id=asdf")).asJava)
    ).asScala
    _ = println("init 333")
    workspaceBuildTargetsResult <- server.workspaceBuildTargets().asScala
    _ <- server.buildShutdown().asScala
    _ = println("init 444")
  yield
    server.onBuildExit()
    println("init 555")
    connection.finishedPromise.success(())
    println("init 665")
