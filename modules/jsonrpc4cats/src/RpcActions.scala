package jsonrpc4cats

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.github.plokhotnyuk.jsoniter_scala.core.WriterConfig

/** Represents a response for a client RPC request.  */
sealed trait RpcResponse[T]

/**
 * Represents a successful client RPC request.
 *
 * @param value is the value that was successfully serialized from `underlying`.
 * @param underlying is the underlying JSON-RPC message where the value comes from.
 */
final case class RpcSuccess[T](
    value: T,
    underlying: Response.Success
) extends RpcResponse[T]

/**
 * Represents a failed client RPC request.
 *
 * @param methodName is the name of the method that failed to complete.
 * @param underlying is the underlying JSON-RPC error message.
 */
final case class RpcFailure[T](
    methodName: String,
    underlying: Response.Error
) extends RuntimeException(RpcFailure.toMsg(methodName, underlying))
    with RpcResponse[T]

object RpcFailure {
  def toMsg(methodName: String, err: Response.Error): String = {
    val errMsg = writeToString(err, config = WriterConfig.withIndentionStep(4))
    s"Unexpected error when calling '$methodName': $errMsg"
  }
}
