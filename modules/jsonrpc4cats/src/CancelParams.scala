package jsonrpc4cats

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

final case class CancelParams(id: RequestId)

object CancelParams:
  given JsonValueCodec[CancelParams] = JsonCodecMaker.make(CodecMakerConfig)
