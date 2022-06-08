package buildsonnet.evaluator

import buildsonnet.ast.{Source, SourceFile}

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
