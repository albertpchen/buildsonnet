package jsonrpc4cats

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util

import scala.util.Try
import scala.util.Success
import scala.util.Failure

final class LowLevelMessage(
  val header: Map[String, String],
  val content: Array[Byte]
) {

  override def equals(obj: scala.Any): Boolean =
    this.eq(obj.asInstanceOf[Object]) || {
      obj match {
        case m: LowLevelMessage =>
          header.equals(m.header) &&
            util.Arrays.equals(content, m.content)
      }
    }

  override def toString: String = {
    val bytes = LowLevelMessageWriter.write(this)
    StandardCharsets.UTF_8.decode(bytes).toString
  }
}

object LowLevelMessage {
  import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray

  def fromMsg(msg: Message): LowLevelMessage = fromBytes(msg.headers, writeToArray(msg))

  def fromBytes(userHeaders: Map[String, String], bytes: Array[Byte]): LowLevelMessage = {
    val headers = userHeaders.filterNot {
      case (key, _) => key.toLowerCase == "content-length"
    } + ("Content-Length" -> bytes.length.toString)
    new LowLevelMessage(headers, bytes)
  }

  def toMsg(message: LowLevelMessage): Message = {
    import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
    // Make sure we propagate headers from the transport to the read message before handling
    Try(readFromArray[Message](message.content)) match {
      case Success(msg: Request) => msg.copy(headers = message.header)
      case Success(msg: Notification) => msg.copy(headers = message.header)
      case Success(msg: Response.Error) => msg.copy(headers = message.header)
      case Success(msg: Response.Success) => msg.copy(headers = message.header)
      case Failure(err) => Response.parseError(err.toString)
    }
  }
}

