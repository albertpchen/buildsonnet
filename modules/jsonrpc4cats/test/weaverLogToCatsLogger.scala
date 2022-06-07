package jsonrpc4cats

import org.typelevel.log4cats.{Logger => CatsLogger}
import weaver.{Expectations, Log => WeaverLog}

def weaverLogToCatsLogger[F[_]](wl: WeaverLog[F]): CatsLogger[F] = new CatsLogger[F] {
  override def error(t: Throwable)(message: => String): F[Unit] = wl.error(msg = message, cause = t)

  override def warn(t: Throwable)(message: => String): F[Unit] = wl.warn(msg = message, cause = t)

  override def info(t: Throwable)(message: => String): F[Unit] = wl.info(msg = message, cause = t)

  override def debug(t: Throwable)(message: => String): F[Unit] = wl.debug(msg = message, cause = t)

  override def trace(t: Throwable)(message: => String): F[Unit] = wl.debug(msg = message, cause = t)

  override def error(message: => String): F[Unit] = wl.error(message)

  override def warn(message: => String): F[Unit] = wl.warn(message)

  override def info(message: => String): F[Unit] = wl.info(message)

  override def debug(message: => String): F[Unit] = wl.debug(message)

  override def trace(message: => String): F[Unit] = wl.debug(message)
}
