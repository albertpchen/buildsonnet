package root

import ch.epfl.scala.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import jsonrpc4s.Endpoint
import jsonrpc4s.Endpoint.unitCodec

object CustomBuildEndpoints {
  final case class CompileReport(
    target: bsp.BuildTargetIdentifier,
    originId: Option[String],
    time: Option[Long],
    noOp: Option[Boolean]
  )

  object CompileReport {
    implicit val codec: JsonValueCodec[CompileReport] =
      JsonCodecMaker.makeWithRequiredCollectionFields
  }
}
