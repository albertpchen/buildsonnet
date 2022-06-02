package buildsonnet.bsp

import buildsonnet.logger.ConsoleLogger
import cats.effect.Sync
import cats.effect.syntax.all.given
import cats.syntax.all.given

import ch.epfl.scala.{bsp => bsp4s}
import ch.epfl.scala.bsp.endpoints
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import jsonrpc4cats.{Services, RawJson}

import org.typelevel.log4cats.Logger

object CustomBuildEndpoints:
  final case class CompileReport(
    target: bsp4s.BuildTargetIdentifier,
    originId: Option[String],
    time: Option[Long],
    noOp: Option[Boolean],
  )

  object CompileReport:
    given JsonValueCodec[CompileReport] =
      JsonCodecMaker.makeWithRequiredCollectionFields

def bspServices[F[_]: Sync: Logger: ConsoleLogger](workspace: String): Services[F] =
  Services
    .empty[F]
    .notification(endpoints.Build.taskStart) { params =>
      params.dataKind match {
        case Some(bsp4s.TaskDataKind.CompileTask) =>
          val report = RawJson.parseJsonTo[CustomBuildEndpoints.CompileReport](params.data.get)
          val id = report.toOption.get.target.uri.value
          report match
            case Left(error) =>
              Logger[F].warn(s"unable to decode compile task data $params: $error").void
            case Right(report) =>
              val id = report.target.uri.value
              val isNoOp = params.message.getOrElse("").startsWith("Start no-op compilation")
              if !isNoOp then
                params.message.fold(().pure)(ConsoleLogger[F].stdout)
              else
                ().pure
        case _ =>
          params.message.fold(().pure)(ConsoleLogger[F].stdout)
      }
    }
    .notification(endpoints.Build.taskProgress)(_.message.fold(().pure)(ConsoleLogger[F].stdout))
    .notification(endpoints.Build.taskFinish)(_.message.fold(().pure)(ConsoleLogger[F].stdout))
    .notification(endpoints.Build.showMessage)(params => ConsoleLogger[F].stdout(params.message))
    .notification(endpoints.Build.logMessage)(params => ConsoleLogger[F].stdout(params.message))
    .notification(endpoints.Build.publishDiagnostics) { params =>
      val file = java.nio.file.Paths.get(workspace).relativize(params.textDocument.uri.toPath)
      params.diagnostics.map { diagnostic =>
        val startLine = diagnostic.range.start.line + 1
        val startCol = diagnostic.range.start.character + 1
        val header = s"${Console.UNDERLINED}$file${Console.RESET}:$startLine:$startCol"
        val logFn =
          diagnostic.severity match
          case Some(bsp4s.DiagnosticSeverity.Error) => ConsoleLogger[F].error
          case Some(bsp4s.DiagnosticSeverity.Warning) => ConsoleLogger[F].warn
          case _ => ConsoleLogger[F].stdout(_: String)
        import ConsoleLogger.prefixLines
        logFn(header) *>
          diagnostic.message.prefixLines("  ").foldLeft(().pure[F])(_ *> logFn(_)) *>
          logFn("")
      }.sequence.void
    }
