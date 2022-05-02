package jsonrpc4cats


import com.github.plokhotnyuk.jsoniter_scala.macros.{JsonCodecMaker, CodecMakerConfig}
import com.github.plokhotnyuk.jsoniter_scala.core.{
  writeToString,
  JsonValueCodec,
  JsonWriter,
  JsonReader,
  WriterConfig,
}

import java.io.{OutputStream, PrintWriter, StringWriter}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, WritableByteChannel}

sealed trait Message {
  def jsonrpc: String

  /**
   * Every message contains a set of HTTP-like headers, but these headers are
   * defined outside of the JSON-RPC protocol specification and are transmitted
   * at the transport level. That is, it is up to the message readers and
   * writers to decide how these headers are trasmitted over the network.
   *
   * For example, if JSON-RPC is being transmitted over HTTP/2, the message
   * reader and writer implementation can use the HTTP/2 headers to transport
   * the message headers. Howevrer, if JSON-RPC messages are transmitted over a
   * socket such as WebSockets or a pipe or file channel, then these headers can
   * be transmitted with the base protocol defined in the Language Server Protocol
   * <a href="https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/">here</a>.
   *
   * This last strategy is the one implemented in this library by default as it
   * is the most flexible regardless of the transport used for JSON-RPC messages.
   *
   * Note that because of this, headers are never serialized into the JSON wire
   * format and the serializers/deserializers here defined will remove any
   * headers before writing and before reading JSON-RPC messages.
   */
  def headers: Map[String, String]
}

object Message {
  given JsonValueCodec[Message] with {
    //                   id method result error params jsonrpc
    // Request           1  2      0      0     0/16   32
    // Response.None     0  0      0      0     0      32
    // Response.Error    1  0      0      8     0      32
    // Response.Success  1  0      4      0     0      32
    // Notification      0  2      0      0     0/16   32
    def decodeValue(in: JsonReader, default: Message): Message = {
      val msg: Message =
        if (in.isNextToken('{')) {
          var p = 63
          var id: RequestId = RequestId.requestIdCodec.nullValue
          var method: String = null
          var result: RawJson = RawJson.codec.nullValue
          var error: ErrorObject = ErrorObject.errorObjectCodec.nullValue
          var params: Option[RawJson] = None
          var jsonrpc: String = null
          if (!in.isNextToken('}')) {
            in.rollbackToken()
            while
              val l = in.readKeyAsCharBuf();
              if (in.isCharBufEqualsTo(l, "id")) {
                p = validateAndSwitchFieldMask(in, l, p, 1)
                id = RequestId.requestIdCodec.decodeValue(in, id)
              } else if (in.isCharBufEqualsTo(l, "method")) {
                p = validateAndSwitchFieldMask(in, l, p, 2)
                method = in.readString(method)
              } else if (in.isCharBufEqualsTo(l, "result")) {
                p = validateAndSwitchFieldMask(in, l, p, 4)
                result = RawJson(in.readRawValAsBytes())
              } else if (in.isCharBufEqualsTo(l, "error")) {
                p = validateAndSwitchFieldMask(in, l, p, 8)
                error = ErrorObject.errorObjectCodec.decodeValue(in, error)
              } else if (in.isCharBufEqualsTo(l, "params")) {
                p = validateAndSwitchFieldMask(in, l, p, 16)
                params = Some(RawJson(in.readRawValAsBytes()))
              } else if (in.isCharBufEqualsTo(l, "jsonrpc")) {
                p = validateAndSwitchFieldMask(in, l, p, 32)
                jsonrpc = in.readString(jsonrpc)
                if (jsonrpc != "2.0") {
                  in.decodeError(
                    s"Expected JSON-RPC version 2.0 message, obtained version $jsonrpc"
                  )
                }
              } else {
                in.skip() // or raise an error here in case of no other fields are not allowed
              }
              (in.isNextToken(','))
            do ()
            if (!in.isCurrentToken('}')) {
              in.objectEndOrCommaError()
            }
          }
          p match {
            case 12 | 28 => Request(method, params, id, Map.empty, jsonrpc)
            case 22 => Response.Error(error, id, jsonrpc)
            case 26 => Response.Success(result, id, jsonrpc)
            case 13 | 29 => Notification(method, params, Map.empty, jsonrpc)
            case _ => default
          }
        } else in.readNullOrTokenError(default, '{')
      if (msg == default) {
        in.decodeError("Invalid JSON-RPC message, expected request, notification or response type")
      }
      msg
    }

    def encodeValue(msg: Message, out: JsonWriter): Unit = {
      msg match {
        case r: Request => Request.requestCodec.encodeValue(r.copy(headers = Map.empty), out)
        case r: Notification =>
          Notification.notificationCodec.encodeValue(r.copy(headers = Map.empty), out)
        case r: Response => Response.responseCodec.encodeValue(r, out)
      }
    }

    def nullValue: Message = null

    private def validateAndSwitchFieldMask(in: JsonReader, l: Int, p: Int, mask: Int): Int = {
      if ((p & mask) != 0) {
        p ^ mask
      } else {
        in.duplicatedKeyError(l)
      }
    }
  }
}

final case class Request(
    method: String,
    params: Option[RawJson],
    id: RequestId,
    /** @inheritdoc */
    headers: Map[String, String],
    jsonrpc: String = "2.0"
) extends Message {
  def toError(code: ErrorCode, message: String): Response =
    Response.error(ErrorObject(code, message, None), id)
}

object Request {
  implicit val requestCodec: JsonValueCodec[Request] =
    JsonCodecMaker.make(CodecMakerConfig.withTransientDefault(false))
}

final case class Notification(
    method: String,
    params: Option[RawJson],
    /** @inheritdoc */
    headers: Map[String, String],
    jsonrpc: String = "2.0"
) extends Message

object Notification {
  implicit val notificationCodec: JsonValueCodec[Notification] =
    JsonCodecMaker.make(CodecMakerConfig.withTransientDefault(false))
}

sealed trait Response extends Message {
  def isSuccess: Boolean = this.isInstanceOf[Response.Success]
}

object Response {
  final case class Success(
      result: RawJson,
      id: RequestId,
      jsonrpc: String = "2.0",
      /** @inheritdoc */
      headers: Map[String, String] = Map.empty
  ) extends Response

  final case class Error(
      error: ErrorObject,
      id: RequestId,
      jsonrpc: String = "2.0",
      /** @inheritdoc */
      headers: Map[String, String] = Map.empty
  ) extends RuntimeException(errorToMsg(id, error, headers))
      with Response

  def errorToMsg(id: RequestId, error: ErrorObject, headers: Map[String, String]): String = {
    case class StringifiedError(id: RequestId, error: ErrorObject, headers: Map[String, String])
    implicit val errorCodec: JsonValueCodec[StringifiedError] =
      JsonCodecMaker.make(CodecMakerConfig.withTransientDefault(false))
    val err = StringifiedError(id, error, headers)
    writeToString(err, config = WriterConfig.withIndentionStep(4))
  }

  implicit val errorCodec: JsonValueCodec[Error] =
    JsonCodecMaker.make(CodecMakerConfig.withTransientDefault(false))
  implicit val successCodec: JsonValueCodec[Success] =
    JsonCodecMaker.make(CodecMakerConfig.withTransientDefault(false))

  implicit val responseCodec: JsonValueCodec[Response] = new JsonValueCodec[Response] {
    def nullValue: Response = null
    def encodeValue(x: Response, out: JsonWriter): Unit = {
      x match {
        case r: Response.Success => successCodec.encodeValue(r.copy(headers = Map.empty), out)
        case r: Response.Error => errorCodec.encodeValue(r.copy(headers = Map.empty), out)
      }
    }

    def decodeValue(in: JsonReader, default: Response): Response = {
      val json = RawJson.codec.decodeValue(in, RawJson.codec.nullValue)
      RawJson.parseJsonTo[Success](json) match {
        case Right(msg) => msg.copy(headers = Map.empty)
        case Left(err) =>
          RawJson.parseJsonTo[Error](json) match {
            case Right(msg) => msg.copy(headers = Map.empty)
            case Left(err) =>
              in.decodeError("Failed to decode JSON-RPC message, missing 'result' or 'error'")
          }
      }
    }
  }

  def ok(result: RawJson, id: RequestId): Response = success(result, id)
  def success(result: RawJson, id: RequestId): Response = Success(result, id)
  def error(error: ErrorObject, id: RequestId): Response.Error = Error(error, id)

  def internalError(err: Throwable, id: RequestId): Response.Error = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    err.printStackTrace(pw)
    val errorMsg = sw.toString
    internalError(errorMsg, id)
  }

  def internalError(message: String): Response.Error =
    internalError(message, RequestId.Null)
  def internalError(message: String, id: RequestId): Response.Error =
    Error(ErrorObject(ErrorCode.InternalError, message, scala.None), id)
  def invalidParams(message: String): Response.Error =
    invalidParams(message, RequestId.Null)
  def invalidParams(message: String, id: RequestId): Response.Error =
    Error(ErrorObject(ErrorCode.InvalidParams, message, scala.None), id)

  def invalidRequest(message: String): Response.Error = {
    Error(
      ErrorObject(ErrorCode.InvalidRequest, message, scala.None),
      RequestId.Null
    )
  }

  def cancelled(id: RequestId): Response.Error = {
    Error(ErrorObject(ErrorCode.RequestCancelled, "", scala.None), id)
  }

  def parseError(message: String): Response.Error =
    Error(ErrorObject(ErrorCode.ParseError, message, scala.None), RequestId.Null)
  def methodNotFound(message: String, id: RequestId): Response.Error =
    Error(ErrorObject(ErrorCode.MethodNotFound, message, scala.None), id)
}
