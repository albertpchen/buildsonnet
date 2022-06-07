package buildsonnet

import cats.effect.{Resource, Sync}
import cats.syntax.all.given
import cats.effect.syntax.all.given

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;
import ch.qos.logback.core.joran.spi.JoranException;
import ch.qos.logback.core.util.StatusPrinter;

import ch.qos.logback.classic.layout.TTLLLayout;
import ch.qos.logback.classic.spi.{Configurator, ILoggingEvent};
import ch.qos.logback.core.{ConsoleAppender, FileAppender};
import ch.qos.logback.core.encoder.LayoutWrappingEncoder;
import ch.qos.logback.core.spi.ContextAwareBase;

import java.nio.file.Path

import org.slf4j.{Logger => JLogger}
import org.slf4j.LoggerFactory

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable.ArrayBuffer

final case class LoggerConfiguration(
  logToConsole: Boolean,
  logToFile: Option[Path],
)

object LoggerConfiguration:
  extension (config: LoggerConfiguration)
    def logger[F[_]: Sync]: Resource[F, Logger[F]] =
      val ctx = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      new BuildsonnetConfigurator(config).configureResource(ctx)

private class BuildsonnetConfigurator[F[_]: Sync](config: LoggerConfiguration) extends ContextAwareBase, Configurator:
  def configure(ctx: LoggerContext): Unit = ???

  def configureResource(ctx: LoggerContext): Resource[F, Logger[F]] =
    if config.logToFile.isEmpty && !config.logToConsole then
      Resource.make(Sync[F].delay {
        ctx.reset()
      })(_ => Sync[F].delay(ctx.stop())).as(Slf4jLogger.getLogger[F])
    else
      val resource = Resource.make[F, List[() => Unit]](Sync[F].delay {
        addInfo("Setting up buildsonnet configuration.")
        val closeFns = ArrayBuffer[() => Unit]()

        if config.logToConsole then
          val consoleAppender = new ConsoleAppender[ILoggingEvent]()
          consoleAppender.setContext(ctx)
          consoleAppender.setName("console")
          val encoder = new LayoutWrappingEncoder[ILoggingEvent]()
          encoder.setContext(ctx)

          val layout = new TTLLLayout()

          layout.setContext(ctx)
          layout.start()
          encoder.setLayout(layout)

          consoleAppender.setEncoder(encoder)
          consoleAppender.start()

          val rootLogger = ctx.getLogger(JLogger.ROOT_LOGGER_NAME)
          rootLogger.addAppender(consoleAppender)

          closeFns.append(() => consoleAppender.stop())

        config.logToFile.foreach { logFile =>
          val fileAppender = new FileAppender[ILoggingEvent]()
          fileAppender.setContext(ctx)
          fileAppender.setName(logFile.toString)
          fileAppender.setFile(logFile.toString)
          fileAppender.setAppend(true)
          fileAppender.setPrudent(true)
          val encoder = new LayoutWrappingEncoder[ILoggingEvent]();
          encoder.setContext(ctx);

          val layout = new TTLLLayout()

          layout.setContext(ctx)
          layout.start()
          encoder.setLayout(layout)

          fileAppender.setEncoder(encoder)
          fileAppender.start()

          val rootLogger = ctx.getLogger(JLogger.ROOT_LOGGER_NAME)
          rootLogger.addAppender(fileAppender)

          closeFns.append(() => fileAppender.stop())
        }

        closeFns.append(() => ctx.stop())
        closeFns.toList
      }) { arr => arr.traverse(fn => Sync[F].delay(fn())).void }
      resource.as(Slf4jLogger.getLogger[F])
