package buildsonnet.evaluator

import buildsonnet.ast.{Source, SourceFile}

final class StackEntry(
  val src: Source,
  val message: String,
):
  override def toString: String =
    val srcString = src match
    case g: Source.Generated => g.file.path
    case src: Source.Range =>
      val (startLine, startCol) = src.file.getLineCol(src.start)
      val (endLine, endCol) = src.file.getLineCol(src.end)
      if startLine == endLine then
        s"${src.file.path}:$startLine:$startCol-$endCol"
      else
        s"${src.file.path}:($startLine:$startCol)-($endLine:$endCol)"
    if message.isEmpty then srcString else s"$srcString $message"

object StackEntry:
  def function(src: Source): StackEntry =
    new StackEntry(src, "function")

  def apply(src: Source): StackEntry =
    new StackEntry(src, "")

  def objectField(src: Source): StackEntry =
    new StackEntry(src, "object")

final class EvaluationError(
  file: SourceFile,
  src: Source,
  message: String,
  stack: List[StackEntry],
) extends Exception(EvaluationError.toString(file, src, message, stack))

object EvaluationError:
  def toString(
    file: SourceFile,
    src: Source,
    message: String,
    stack: List[StackEntry],
  ): String =
    val stackSuffix = (StackEntry(src) +: stack).mkString("\n  ", "\n  ", "")
    s"$message$stackSuffix"
