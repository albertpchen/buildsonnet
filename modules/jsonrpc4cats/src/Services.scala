package jsonrpc4cats

import cats.effect.Sync
import cats.effect.syntax.all.given
import cats.syntax.all.given
import scala.util.{Try, Success, Failure}
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import org.typelevel.log4cats.Logger

final case class Service[F[_]](
  methodName: String,
  handle: Message => F[Option[Response]],
)

final class PartiallyAppliedService[F[_]: Sync: Logger]:
  def request[A, B](endpoint: Endpoint[A, B])(service: A => F[B]): Service[F] =
    Service.request[F, A, B](endpoint)(service)

  def notification[A](endpoint: Endpoint[A, Unit])(service: A => F[Unit]): Service[F] =
    Service.notification[F, A](endpoint)(service)

object Service:
  def apply[F[_]: Sync: Logger]: PartiallyAppliedService[F] = PartiallyAppliedService[F]()

  def request[F[_]: Sync, A, B](endpoint: Endpoint[A, B])(service: A => F[B]): Service[F] =
    Service[F](
      endpoint.method,
      message => {
        import endpoint.{codecA, codecB}
        val method = endpoint.method
        val response = message match {
        case Request(`method`, params, id, jsonrpc, _) =>
          val paramsJson = params.getOrElse(RawJson.nullValue)
          Sync[F]
            .delay(readFromArray[A](paramsJson.value))
            .flatMap { value =>
              service(value)
                .map { response =>
                  Response.ok(RawJson.toJson(response), id)
                }
                .handleErrorWith {
                  // Errors always have a null id because services don't have access to the real id
                  case err: Response.Error => Sync[F].pure(err.copy(id = id))
                  case err => Sync[F].pure(Response.internalError(err, id))
                }
            }
            .handleErrorWith(err => Sync[F].pure(Response.invalidParams(err.toString, id)))
        case Request(invalidMethod, _, id, _, _) =>
          Sync[F].pure(Response.methodNotFound(invalidMethod, id))
        case _ => Sync[F].pure(Response.invalidRequest(s"Expected request, obtained $message"))
        }
        response.map(Some(_))
      },
    )

  def notification[F[_]: Sync: Logger, A](
    endpoint: Endpoint[A, Unit],
  )(service: A => F[Unit]): Service[F] = {
    Service[F](
      endpoint.method,
      message => {
        import endpoint.codecA
        val method = endpoint.method
        val program = message match {
        case Notification(`method`, params, _, _) =>
          val paramsJson = params.getOrElse(RawJson.nullValue)
          Sync[F]
            .delay(readFromArray[A](paramsJson.value))
            .flatMap(service)
            .handleErrorWith(err =>
              Logger[F].error(s"Failed to parse notification $message. Errors: $err"),
            )
        case Notification(invalidMethod, _, _, _) =>
          Logger[F].error(s"Expected method '$method', obtained '$invalidMethod'")
        case _ => Logger[F].error(s"Expected notification, obtained $message")
        }
        program *> None.pure
      },
    )
  }

object Services:
  def empty[F[_]: Sync: Logger]: Services[F] = new Services[F](Nil)

class Services[F[_]: Sync: Logger] private (
  val services: List[Service[F]],
):
  def request[A, B](endpoint: Endpoint[A, B])(f: A => F[B]): Services[F] =
    addService(Service.request[F, A, B](endpoint)(f))

  def notification[A](endpoint: Endpoint[A, Unit])(f: A => F[Unit]): Services[F] =
    addService(Service.notification[F, A](endpoint)(f))

  def byMethodName: Map[String, Service[F]] =
    services.iterator.map(s => s.methodName -> s).toMap

  def addService(service: Service[F]): Services[F] = {
    val duplicate = services.find(_.methodName == service.methodName)
    require(
      duplicate.isEmpty,
      s"Duplicate service handler for method ${duplicate.get.methodName}",
    )
    new Services(service :: services)
  }
