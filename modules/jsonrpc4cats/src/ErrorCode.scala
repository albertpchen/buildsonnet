package jsonrpc4cats

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

enum ErrorCode(val value: Int):
  case ParseError extends ErrorCode(-32700)
  case InvalidRequest extends ErrorCode(-32600)
  case MethodNotFound extends ErrorCode(-32601)
  case InvalidParams extends ErrorCode(-32602)
  case InternalError extends ErrorCode(-32603)
  case RequestCancelled extends ErrorCode(-32800)

  // Server error: -32000 to -32099
  case Unknown(override val value: Int) extends ErrorCode(value)

case object ErrorCode {

  val builtin: Array[ErrorCode] = Array(
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    RequestCancelled
  )

  given errorCodeCodec: JsonValueCodec[ErrorCode] = new JsonValueCodec[ErrorCode] {
    def nullValue: ErrorCode = null
    def encodeValue(x: ErrorCode, out: JsonWriter): Unit = out.writeVal(x.value)
    def decodeValue(in: JsonReader, default: ErrorCode): ErrorCode = {
      val x = in.readInt()
      builtin.find(_.value == x).getOrElse(Unknown(x))
    }
  }
}

