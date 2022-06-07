package jsonrpc4cats

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException

enum RequestId:
  case Null
  case Number(value: Double)
  case String(value: java.lang.String)

object RequestId:
  def apply(n: Int): RequestId = RequestId.Number(n.toDouble)
  def apply(n: Long): RequestId = RequestId.Number(n.toDouble)

  given requestIdCodec: JsonValueCodec[RequestId] = new JsonValueCodec[RequestId] {
    def nullValue: RequestId = RequestId.Null
    def decodeValue(in: JsonReader, default: RequestId): RequestId = {
      in.setMark()
      if (in.isNextToken('"')) {
        in.rollbackToken()
        val str = in.readString(null)
        if (str == null) RequestId.Null
        else RequestId.String(str)
      } else {
        in.rollbackToMark()
        try RequestId.Number(in.readDouble())
        catch {
          case _: JsonReaderException =>
            in.rollbackToMark()
            in.readNullOrError("", "Expected valid JSON-RPC request id: string, numnber or null")
            // If null read succeeds, return it, otherwise error will be thrown
            RequestId.Null
        }
      }
    }

    def encodeValue(x: RequestId, out: JsonWriter): Unit = {
      x match {
      case Null => out.writeNull()
      case Number(n) =>
        if (n % 1 == 0) out.writeVal(n.toLong)
        else out.writeVal(n)
      case String(s) => out.writeVal(s)
      }
    }
  }
