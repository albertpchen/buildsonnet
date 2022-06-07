package buildsonnet.bsp

import cats.effect.{Async, Resource, Sync}
import cats.effect.std.Dispatcher
import cats.effect.syntax.all.given
import cats.syntax.all.given

import bloop.launcher.Launcher
import bloop.launcher.LauncherMain

import java.io.{InputStream, OutputStream}
import java.nio.channels.{Channels, Pipe}
import java.nio.file.Path

import org.typelevel.log4cats.Logger

import scala.concurrent.{ExecutionContext, Promise}
import scala.concurrent.duration.Duration

case class SocketConnection[F[_]](
  output: F[OutputStream],
  input: F[InputStream],
)(private[bsp] val cancel: F[Unit])

object SocketConnection:
  def connectToLauncher[F[_]: Async: Logger](
    bloopVersion: String,
    bloopPort: Int,
  ): Resource[F, SocketConnection[F]] =
    Resource.make(Dispatcher[F].use { dispatcher =>
      for
        ec <- Async[F].executionContext
        connection <- Async[F].fromFuture(Async[F].delay {
          val launcherInOutPipe = Pipe.open()
          val launcherIn = Channels.newInputStream(launcherInOutPipe.source())
          val clientOut = Channels.newOutputStream(launcherInOutPipe.sink())

          val clientInOutPipe = Pipe.open()
          val clientIn = Channels.newInputStream(clientInOutPipe.source())
          val launcherOut = Channels.newOutputStream(clientInOutPipe.sink())

          val serverStarted = Promise[Unit]()
          val (logStream, logStreamFiber) = {
            val pipe = Pipe.open()
            val is = Channels.newInputStream(pipe.source())
            val os = Channels.newOutputStream(pipe.sink())

            val fiber = dispatcher.unsafeRunSync(
              Async[F]
                .blocking(
                  fs2
                    .io
                    .readInputStream(is.pure, chunkSize = 8192, closeAfterUse = true)
                    .through(fs2.text.utf8.decode)
                    .through(fs2.text.lines)
                    .foreach(line => Logger[F].info(line))
                    .compile
                    .drain,
                )
                .start,
            )
            (java.io.PrintStream(os, true), fiber)
          }

          val launcher = new LauncherMain(
            launcherIn,
            launcherOut,
            logStream,
            java.nio.charset.StandardCharsets.UTF_8,
            bloop.bloopgun.core.Shell.default,
            userNailgunHost = None,
            userNailgunPort = Some(bloopPort),
            serverStarted,
          )

          val job = dispatcher.unsafeRunSync(
            Async[F].blocking {
              launcher.runLauncher(
                bloopVersion,
                skipBspConnection = false,
                Nil,
              )
            }.start,
          )

          serverStarted
            .future
            .map { _ =>
              SocketConnection(clientOut.pure, clientIn.pure)(Sync[F].defer {
                clientOut.flush()
                clientOut.close()
                logStreamFiber.cancel *> job.cancel
              })
            }(ec)
        })
      yield connection
    })(_.cancel)
