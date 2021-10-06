package root

import java.io.PrintStream

import scala.Console.{CYAN, GREEN, RED, RESET, YELLOW}
import scala.util.matching.Regex

object Logger:
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

  val lineSeparator: String = Option(System.getenv("SHELL")) match {
    case Some(currentShell) if validShells.exists(sh => currentShell.contains(sh)) => "\n"
    case _ => System.getProperty("line.separator", "\n")
  }

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

  /**
   * Instantiates a new `BloopLogger` using the specified streams.
   *
   * @param name       The name of the logger.
   * @param out        The stream to use to write `INFO` and `WARN` level messages.
   * @param err        The stream to use to write `FATAL`, `ERROR`, `DEBUG` and `TRACE` level messages.
   * @param isVerbose  Tells whether the logger is verbose or not.
   * @param filter     Filters that apply to all debug messages.
   * @return A `BloopLogger` whose output will be written in the specified streams.
   */
  def at(
      name: String,
      out: PrintStream,
      err: PrintStream,
      isVerbose: Boolean,
      colorOutput: Boolean,
  ): Logger = {
    new Logger(name, out, err, if (isVerbose) 1 else 0, colorOutput, None)
  }

  /**
   * Instantiates a new `BloopLogger` using the specified streams.
   *
   * @param name       The name of the logger.
   * @param out        The stream to use to write `INFO` and `WARN` level messages.
   * @param err        The stream to use to write `FATAL`, `ERROR`, `DEBUG` and `TRACE` level messages.
   * @param filter     Filters that apply to all debug messages.
   * @return A `BloopLogger` whose output will be written in the specified streams.
   */
  def at(
      name: String,
      out: PrintStream,
      err: PrintStream,
      colorOutput: Boolean,
  ): Logger = at(name, out, err, false, colorOutput)

  /**
   * Instantiates a new `BloopLogger` that writes to stdout and stderr.
   *
   * @param name The name of the logger.
   * @return A `BloopLogger` writing to stdout and stderr. Calling this method is equivalent to
   *         calling `at(name, System.out, System.err)`.
   */
  def default(name: String): Logger =
    at(name, System.out, System.err, true)

  def prettyPrintException(t: Throwable): String = {
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)
    sw.toString()
  }

  private lazy val colorsRegex = "\u001b\\[[0-9;]*m".r

  /**
   * Remove the ANSI colors from `str`.
   *
   * Other ANSI escapes codes are left untouched.
   */
  private def stripColors(str: String): String = {
    colorsRegex.replaceAllIn(str, "")
  }


/**
 * Creates a logger that writes to the given streams.
 *
 * @param name        The name of this logger.
 * @param out         The stream to use to write `INFO` and `WARN` level messages.
 * @param err         The stream to use to write `FATAL`, `ERROR`, `DEBUG` and `TRACE` level messages.
 * @param colorOutput Print with or without color.
 */
final class Logger(
    val name: String,
    out: PrintStream,
    err: PrintStream,
    private val debugCount: Int,
    colorOutput: Boolean,
    originId: Option[String]
) {
  import Logger._

  def error(msg: String): Unit = print(msg, printError)
  def warn(msg: String): Unit = print(msg, printWarning)
  def trace(exception: Throwable): Unit = trace("", exception)
  def info(msg: String): Unit = print(msg, printInfo)

  def asDiscrete: Logger = {
    if (debugCount <= 0) this
    else new Logger(name, out, err, debugCount - 1, colorOutput, originId)
  }

  def isVerbose: Boolean = debugCount > 0
  def asVerbose: Logger = {
    new Logger(name, out, err, debugCount + 1, colorOutput, originId)
  }

  def withOriginId(originId: Option[String]): Logger =
    new Logger(name, out, err, debugCount, colorOutput, originId)

  @scala.annotation.tailrec
  private def trace(prefix: String, exception: Throwable): Unit = {
    if (isVerbose) {
      print(prefix + exception.toString, printTrace)
      exception.getStackTrace.foreach(ste => print("\t" + ste.toString, printTrace))

      val cause = exception.getCause
      if (cause != null) trace("Caused by: ", cause)
    }
  }

  private def print(msg: String, fn: String => Unit): Unit = {
    val lines = msg.splitLines
    val printFn = if (colorOutput) fn else (str: String) => fn(Logger.stripColors(str))
    lines.foreach(printFn)
  }

  private def printInfo(line: String): Unit = {
    out.print(line + lineSeparator)
  }

  private def colored(color: String, msg: String): String = {
    if (colorOutput)
      s"${RESET}${color}$msg${RESET}"
    else
      msg
  }

  private def printWarning(line: String): Unit = {
    out.println(s"${colored(YELLOW, "[W]")} $line")
  }

  private def printError(line: String): Unit = {
    err.println(s"${colored(RED, "[E]")} $line")
  }

  private def printTrace(line: String): Unit = {
    err.println(s"${colored(CYAN, "[T]")} $line")
  }

  private[root] def printDebug(line: String): Unit = {
    if (!isVerbose) ()
    else err.println(s"${colored(GREEN, "[D]")} $line")
  }
}
