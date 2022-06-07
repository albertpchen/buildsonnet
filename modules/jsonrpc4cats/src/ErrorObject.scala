package jsonrpc4cats

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

case class ErrorObject(
    code: ErrorCode,
    message: String,
    data: Option[RawJson]
)

object ErrorObject {
  given errorObjectCodec: JsonValueCodec[ErrorObject] =
    JsonCodecMaker.make(CodecMakerConfig)
}

