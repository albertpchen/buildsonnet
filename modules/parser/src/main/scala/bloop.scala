package root

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import java.nio.channels.Channels
import java.nio.channels.Pipe
import bloop.launcher.Launcher
/*
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
)(using ec: ExecutionContextExecutorService): Future[SocketConnection] = {
  val launcherInOutPipe = Pipe.open()
  // val launcherIn = new QuietInputStream(
  //   Channels.newInputStream(launcherInOutPipe.source()),
  //   "Bloop InputStream"
  // )
  val launcherIn = Channels.newInputStream(launcherInOutPipe.source())
  // val clientOut = new ClosableOutputStream(
  //   Channels.newOutputStream(launcherInOutPipe.sink()),
  //   "Bloop OutputStream"
  // )
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
  val launcherTmpDir = Files.createTempDirectory(s"bsp-launcher")
  val bspBridge = new BspBridge(
    launcherIn,
    launcherOut,
    System.err,
    Promise[Unit](),
    System.err,
    bloop.bloopgun.core.Shell.default,
    launcherTmpDir,
  )
  Launcher.connectToBloopBspServer("1.1.2", false, bspBridge, List()).flatten.flatMap {
    _.fold(Left(""))
  }
  println("222")

  val finished = Promise[Unit]()
  val job = ec.submit(new Runnable {
    override def run(): Unit = {
      launcher.runLauncher(
        bloopVersion,
        skipBspConnection = true,
        Nil
      )
      serverStarted.success(())
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
          clientOut.flush()
          clientOut.close()
        },
        Cancelable(() => job.cancel(true))
      ),
      finished
    )
  }
}
*/
