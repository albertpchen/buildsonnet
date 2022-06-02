package buildsonnet.logger

import cats.effect.Sync
import cats.syntax.all.given

import java.io.PrintStream

import scala.Console.{CYAN, GREEN, RED, RESET, YELLOW}
import scala.util.matching.Regex

object ConsoleLogger:
  private lazy val validShells = Seq(
    "/bin/sh",
    "/bin/ash",
    "/bin/bash",
    "/bin/dash",
    "/bin/mksh",
    "/bin/pdksh",
    "/bin/posh",
    "/bin/tcsh",
    "/bin/zsh",
    "/bin/fish"
  )

  val lineSeparator: String = Option(System.getenv("SHELL")) match
    case Some(currentShell) if validShells.exists(sh => currentShell.contains(sh)) => "\n"
    case _ => System.getProperty("line.separator", "\n")

  /**
   * Regular expression that reliably splits Strings into lines.
   *
   * Effective for all commonly encountered line endings:
   *  - "\n"
   *  - "\r\n"
   *  - "\r"
   */
  def END_OF_LINE_MATCHER = "\r\n|\n"

  extension (str: String)
    /*
     * The safe general way to split Strings to lines.   Makes minimal assumptions
     * about the source of the String.
     *
     * Does not depend on editor end-of-line configuration.
     * Correctly splits strings from any of the common OSTYPES.
     */
    def splitLines: Array[String] = str.split(END_OF_LINE_MATCHER)

    def prefixLines(prefix: String): Array[String] = str.splitLines.map(prefix + _)

  private lazy val colorsRegex = "\u001b\\[[0-9;]*m".r

  /**
   * Remove the ANSI colors from `str`.
   *
   * Other ANSI escapes codes are left untouched.
   */
  private def stripColors(str: String): String =
    colorsRegex.replaceAllIn(str, "")

  /**
   * Instantiates a new `BloopLogger` that writes to stdout and stderr.
   *
   * @param name The name of the logger.
   * @return A `BloopLogger` writing to stdout and stderr. Calling this method is equivalent to
   *         calling `at(name, System.out, System.err)`.
   */
  def default[F[_]: Sync](name: String): ConsoleLogger[F] =
    val isVerbose = true
    new ConsoleLogger[F](
      name = name,
      out = Printer(System.out),
      err = Printer(System.err),
      debugCount = if isVerbose then 1 else 0,
      colorOutput = true,
      originId = None,
    )

  def apply[F[_]: ConsoleLogger]: ConsoleLogger[F] = summon

sealed trait Printer[F[_]]:
  def print(str: String): F[Unit]
  def println(str: String): F[Unit]
  def printStream: PrintStream

object Printer:
  def apply[F[_]: Sync](stream: PrintStream): Printer[F] = new Printer[F] {
    def print(str: String): F[Unit] = Sync[F].delay(stream.print(str))
    def println(str: String): F[Unit] = Sync[F].delay(stream.println(str))
    val printStream = stream
  }

/**
 * Creates a logger that writes to the given streams.
 *
 * @param name        The name of this logger.
 * @param out         The stream to use to write `INFO` and `WARN` level messages.
 * @param err         The stream to use to write `FATAL`, `ERROR`, `DEBUG` and `TRACE` level messages.
 * @param colorOutput Print with or without color.
 */
final class ConsoleLogger[F[_]: Sync](
    val name: String,
    out: Printer[F],
    err: Printer[F],
    private val debugCount: Int,
    colorOutput: Boolean,
    originId: Option[String],
) {
  def error(msg: String): F[Unit] = print(msg, printError)
  def warn(msg: String): F[Unit] = print(msg, printWarning)
  def stdout(msg: String): F[Unit] = print(msg, line => out.print(line + ConsoleLogger.lineSeparator))
  def stderr(msg: String): F[Unit] = print(msg, line => err.print(line + ConsoleLogger.lineSeparator))
  def outStream: PrintStream = out.printStream
  def errStream: PrintStream = err.printStream

  def isVerbose: Boolean = debugCount > 0
  def asVerbose: ConsoleLogger[F] =
    new ConsoleLogger[F](name, out, err, debugCount + 1, colorOutput, originId)

  import ConsoleLogger.splitLines
  private def print(msg: String, fn: String => F[Unit]): F[Unit] =
    val lines = msg.splitLines
    val printFn = if colorOutput then fn else (str: String) => fn(ConsoleLogger.stripColors(str))
    lines.foldLeft(().pure[F])(_ *> printFn(_))

  private def colored(color: String, msg: String): String =
    if (colorOutput)
      s"${RESET}${color}$msg${RESET}"
    else
      msg

  private def printWarning(line: String): F[Unit] =
    out.println(s"${colored(YELLOW, "[W]")} $line")

  private def printError(line: String): F[Unit] =
    err.println(s"${colored(RED, "[E]")} $line")

  private def printTrace(line: String): F[Unit] =
    err.println(s"${colored(CYAN, "[T]")} $line")

  private[logger] def printDebug(line: String): F[Unit] =
    if !isVerbose then ().pure
    else err.println(s"${colored(GREEN, "[D]")} $line")
}
