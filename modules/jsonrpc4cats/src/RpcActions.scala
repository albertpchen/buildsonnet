package jsonrpc4cats

import cats.Apply

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.github.plokhotnyuk.jsoniter_scala.core.WriterConfig

/** Represents a response for a client RPC request. */
sealed trait RpcResponse[T]

object RpcResponse:
  extension [T](response: RpcResponse[T])
    def toEither: Either[RpcFailure[T], RpcSuccess[T]] =
      response match
      case fail: RpcFailure[T] => Left(fail)
      case success: RpcSuccess[T] => Right(success)

    def foldValue[A](fError: String => A)(fValue: T => A): A =
      response match
      case fail: RpcFailure[T] => fError(fail.getMessage)
      case RpcSuccess(value, _) => fValue(value)

  given Apply[RpcResponse] with
    def ap[A, B](ff: RpcResponse[A => B])(fa: RpcResponse[A]): RpcResponse[B] =
      (ff, fa) match
      case (RpcSuccess(ff, _), RpcSuccess(fa, underlying)) => RpcSuccess(ff(fa), underlying)
      case (RpcFailure(methodName, underlying), _) => RpcFailure(methodName, underlying)
      case (_, RpcFailure(methodName, underlying)) => RpcFailure(methodName, underlying)

    def map[A, B](fa: RpcResponse[A])(f: A => B): RpcResponse[B] =
      fa match
      case RpcFailure(methodName, underlying) => RpcFailure(methodName, underlying)
      case RpcSuccess(value, underlying) => RpcSuccess(f(value), underlying)

/** Represents a successful client RPC request.
  *
  * @param value
  *   is the value that was successfully serialized from `underlying`.
  * @param underlying
  *   is the underlying JSON-RPC message where the value comes from.
  */
final case class RpcSuccess[T](
  value: T,
  underlying: Response.Success,
) extends RpcResponse[T]

/** Represents a failed client RPC request.
  *
  * @param methodName
  *   is the name of the method that failed to complete.
  * @param underlying
  *   is the underlying JSON-RPC error message.
  */
final case class RpcFailure[T](
  methodName: String,
  underlying: Response.Error,
) extends RuntimeException(RpcFailure.toMsg(methodName, underlying))
    with RpcResponse[T]

object RpcFailure {
  def toMsg(methodName: String, err: Response.Error): String = {
    val errMsg = writeToString(err, config = WriterConfig.withIndentionStep(4))
    s"Unexpected error when calling '$methodName': $errMsg"
  }
}
