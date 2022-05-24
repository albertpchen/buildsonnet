package buildsonnet.bsp

import cats.effect.{Async, Resource, Sync}
import cats.effect.std.Dispatcher
import cats.effect.syntax.all.given
import cats.syntax.all.given

import bloop.launcher.Launcher
import bloop.launcher.LauncherMain

import java.nio.channels.{Channels, Pipe}

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import scala.concurrent.duration.Duration

case class SocketConnection[F[_]](
  output: F[java.io.OutputStream],
  input: F[java.io.InputStream],
)(private[bsp] val cancel: F[Unit])

object SocketConnection:
  def connectToLauncher[F[_]: Async](
    bloopVersion: String,
    bloopPort: Int,
    logStream: java.io.PrintStream,
  ): Resource[F, SocketConnection[F]] =
    for
      dispatcher <- Dispatcher[F]
      connection <- Resource.make(Sync[F].interruptible {
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

        val cancelJob = dispatcher.unsafeRunCancelable(Sync[F].delay {
          launcher.runLauncher(
            bloopVersion,
            skipBspConnection = false,
            Nil
          )
        })

        dispatcher.unsafeRunTimed(serverStarted.pure, Duration.Inf)
        SocketConnection(clientOut.pure, clientIn.pure)(Sync[F].delay {
          clientOut.flush()
          clientOut.close()
          cancelJob()
        })
      })(_.cancel)
    yield
      connection
