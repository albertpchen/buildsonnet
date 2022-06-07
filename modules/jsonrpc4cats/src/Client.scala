package jsonrpc4cats

import cats.{Monad, MonadError}
import cats.effect.{
  Async,
  Deferred,
  Fiber,
  MonadCancel,
  MonadCancelThrow,
  Resource,
  Ref,
  Sync,
  Temporal,
}
import cats.effect.std.{Queue, Semaphore}
import cats.effect.syntax.all.given
import cats.syntax.all.given

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromArray,
  writeToArray,
  JsonValueCodec,
}

import fs2.Stream

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.channels.{Channels, WritableByteChannel}
import java.nio.charset.StandardCharsets

import org.typelevel.log4cats.Logger


sealed trait RpcClient[F[_]]:
  def notify[A](
    endpoint: Endpoint[A, Unit],
    params: A,
    headers: Map[String, String] = Map.empty
  ): F[Unit]

  def request[A, B](
    endpoint: Endpoint[A, B],
    params: A,
    headers: Map[String, String] = Map.empty
  ): F[RpcResponse[B]]

  def messageStream: Stream[F, Message]

  private[jsonrpc4cats] def serverRespond(response: Response): F[Unit]

  private[jsonrpc4cats] def clientRespond(response: Response): F[Unit]

  private[jsonrpc4cats] def cancel(): F[Unit]


object RpcClient:
  def setupBytes[F[_]: Async: Logger](
    in: Stream[F, Byte],
    services: Services[F],
    maxConcurrentServiceWorkers: Int,
  ): Resource[F, RpcClient[F]] =
    setup(
      LowLevelMessageReader
        .toMessageStream(in)
        .map(LowLevelMessage.toMsg),
      services,
      maxConcurrentServiceWorkers,
    )

  def setup[F[_]: Async: Logger](
    in: Stream[F, Message],
    services: Services[F],
    maxConcurrentServiceWorkers: Int,
  ): Resource[F, RpcClient[F]] =
    for
      client <- RpcClient[F]
      server <- RpcServer(services, client, in)
      _ <- server.runBackground(maxConcurrentServiceWorkers)
    yield
      client


  def apply[F[_]: Async: Logger]: Resource[F, RpcClient[F]] = {
    Resource.make[F, RpcClient[F]](
      for
        counter <- Ref[F].of(0)
        activeServerRequests <- Ref[F].of(Map.empty[RequestId, Deferred[F, Response]])
        queue <- Queue.unbounded[F, Option[Message]]
      yield
        new RpcClient[F] {
          def cancel(): F[Unit] =
            for
              requests <- activeServerRequests.getAndSet(Map.empty)
              _ <- requests.map { (id, deferred) =>
                deferred.complete(Response.cancelled(id))
              }.toList.sequence
              _ <- queue.offer(None)
            yield ()

          val messageStream: Stream[F, Message] =
            Stream.fromQueueNoneTerminated(queue)

          private def writeMessage(msg: Message): F[Unit] =
            queue.offer(Some(msg))

          private def toJson[R: JsonValueCodec](r: R): RawJson = RawJson(writeToArray(r))

          def serverRespond(response: Response): F[Unit] = writeMessage(response)

          def clientRespond(response: Response): F[Unit] =
            val id = response match
              case Response.Success(_, requestId, jsonrpc, _) => requestId
              case Response.Error(_, requestId, jsonrpc, _) => requestId
            for
              _ <- Logger[F].info(s"Client got response: $response")
              requests <- activeServerRequests.getAndUpdate(_ - id)
              deferred <- requests.get(id).fold(
                Logger[F].error(s"Response to unknown request: $response")
              )(_.complete(response).void)
            yield ()

          def notify[A](
            endpoint: Endpoint[A, Unit],
            params: A,
            headers: Map[String, String] = Map.empty,
          ): F[Unit] = {
            val json = toJson(params)(using endpoint.codecA)
            val msg = Notification(endpoint.method, Some(json), headers)
            writeMessage(msg)
          }

          def request[A, B](
            endpoint: Endpoint[A, B],
            params: A,
            headers: Map[String, String] = Map.empty,
          ): F[RpcResponse[B]] = {
            import endpoint.{codecA, codecB}
            for
              reqId <- counter.getAndUpdate(_ + 1).map(RequestId(_))
              json = Request(endpoint.method, Some(toJson(params)(using endpoint.codecA)), reqId, headers)
              derferredResponse <- Deferred[F, Response]
              _ <- activeServerRequests.update(_ + (reqId -> derferredResponse))
              _ <- writeMessage(json)
              cancellation = Response.cancelled(reqId)
              response <- derferredResponse.get.onCancel(
                activeServerRequests
                  .get
                  .flatMap { requests =>
                    requests
                      .get(reqId)
                      .fold(().pure)(_.complete(cancellation).void)
                  }
              )
              rpcResponse <- response match {
                case err: Response.Error => Sync[F].pure[RpcResponse[B]](RpcFailure(endpoint.method, err))
                case suc: Response.Success =>
                  Sync[F]
                    .delay[RpcResponse[B]](RpcSuccess(readFromArray[B](suc.result.value), suc))
                    .handleErrorWith { err =>
                      Sync[F].pure(RpcFailure(endpoint.method, Response.invalidParams(err.toString, reqId)))
                    }
              }
            yield rpcResponse
          }
        }
      )(_.cancel())
  }
