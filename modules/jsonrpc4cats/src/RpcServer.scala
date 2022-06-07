package jsonrpc4cats

import cats.{Monad, MonadError}
import cats.effect.{Async, Deferred, Sync, Spawn, Fiber, Ref, Resource}
import cats.effect.std.Supervisor
import cats.effect.syntax.all.given
import cats.syntax.all.given

import fs2.{Pull, Stream}

import org.typelevel.log4cats.Logger

import scala.util.control.NonFatal

sealed trait RpcServer[F[_]]:
  def runUntilCancelled(maxConcurrent: Int): F[F[Unit]]

  private[jsonrpc4cats] def cancel: F[Unit]

object RpcServer:
  import Endpoint.unitCodec
  val cancelRequest = Endpoint[CancelParams, Unit]("$/cancelRequest")

  extension [F[_]: Async](server: RpcServer[F])
    def runBackground(maxConcurrentServiceWorkers: Int): Resource[F, Unit] =
      Resource.make(server.runUntilCancelled(maxConcurrentServiceWorkers))(identity).void

  private def applyImpl[F[_]: Async: Logger](
    services: Services[F],
    client: RpcClient[F],
    supervisor: Supervisor[F],
    activeClientRequests: Ref[F, Map[RequestId, Fiber[F, Throwable, Unit]]],
    in: Stream[F, Message],
  ): RpcServer[F] = new RpcServer[F] {
    def cancel: F[Unit] =
      for
        activeClientRequests <- activeClientRequests.getAndSet(Map.empty)
        _ <- activeClientRequests.toList.traverse(_._2.cancel)
      yield ()

    val cancelNotification: Service[F] =
      Service.notification[F, CancelParams](cancelRequest) { (params: CancelParams) =>
        val id = params.id
        activeClientRequests
          .getAndUpdate(_ - id)
          .map(_.get(id))
          .flatMap {
            case None => Logger[F].warn(s"Can't cancel request $id, no active request found.")
            case Some(request) =>
              for
                _ <- Logger[F].info(s"Cancelling request $id")
                _ <- request.cancel
              yield Response.cancelled(id)
          }
      }

    val handlersByMethodName: Map[String, Service[F]] =
      services.addService(cancelNotification).byMethodName

    def handleResponse(response: Response): F[Unit] =
      client.clientRespond(response)

    def handleRequest(request: Request): F[Unit] =
      val Request(method, _, id, _, _) = request
      handlersByMethodName.get(method) match
      case None => Logger[F].info(s"Method not found '$method'")
      case Some(handler) =>
        for
          responseFiber <- supervisor.supervise(
            handler
              .handle(request)
              .handleErrorWith {
                case NonFatal(e) =>
                  Logger[F].error(s"Unhandled JSON-RPC error handling request $request: $e") *>
                    Sync[F].pure(Some(Response.internalError(e.getMessage, id)))
                case e => e.raiseError
              }
              .flatMap {
                case None =>
                  Logger[F]
                    .error(
                      s"Unhandled JSON-RPC error handler for request $request did not return a response",
                    )
                    .as(
                      Response.internalError(s"response for method $method is not implemented", id),
                    )
                case Some(response) => response.pure
              }
              .flatMap(client.serverRespond),
          )
          _ <- activeClientRequests.update(_ + (id -> responseFiber))
        yield ()

    def handleNotification(notification: Notification): F[Unit] =
      val Notification(method, _, _, _) = notification
      handlersByMethodName.get(method) match
      case None => Logger[F].info(s"Unknown method '$method'")
      case Some(handler) =>
        for
          response <- handler
            .handle(notification)
            .handleErrorWith {
              case NonFatal(e) =>
                Logger[F].error(s"Error handling notification $notification: $e").as(None)
              case e => e.raiseError
            }
          result <- response match
          case None => ().pure
          case Some(nonEmpty) =>
            Logger[F].error(
              s"Obtained non-empty response $nonEmpty for notification $notification!",
            )
        yield ()

    def runUntilCancelled(maxConcurrent: Int): F[F[Unit]] =
      for
        cancel <- Deferred[F, Unit]
        _ <- in
          .interruptWhen(cancel.get.attempt)
          .parEvalMap(maxConcurrent) {
            case response: Response => handleResponse(response).void
            case notification: Notification => handleNotification(notification).void
            case request: Request => handleRequest(request)
          }
          .compile
          .drain
          .start
      yield cancel.complete(()).void
  }

  def apply[F[_]: Async: Logger](
    services: Services[F],
    client: RpcClient[F],
    in: Stream[F, Message],
  ): Resource[F, RpcServer[F]] =
    for
      supervisor <- Supervisor[F]
      server <- Resource.make(
        Ref[F]
          .of(Map.empty[RequestId, Fiber[F, Throwable, Unit]])
          .map(applyImpl(services, client, supervisor, _, in)),
      )(_.cancel)
    yield server
