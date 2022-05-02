package jsonrpc4cats

import cats.Show
import cats.effect.{Deferred, IO, Outcome, Ref}
import cats.effect.std.{Queue}
import cats.effect.syntax.all.given
import cats.syntax.all.given

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import fs2.Stream

import jsonrpc4cats.testprotocol.*

import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.log4cats.{Logger => CatsLogger}

import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}


object SimpleTest extends SimpleIOSuite with Checkers:

  inline given [T]: JsonValueCodec[T] = JsonCodecMaker.makeWithRequiredCollectionFields[T]
  val openConnection = Endpoint.request[OpenConnectionParams, OpenConnectionReturn]("openConnection")
  val compile = Endpoint.request[CompileParams, CompileReturn]("compile")
  val warn = Endpoint.notification[WarnParams]("warn")

  def runTestActions(
    testActions: Array[TestAction],
  )(using CatsLogger[IO]): IO[Expectations] = {
    for
      inQueue <- Queue.bounded[IO, Option[Message]](1)
      expectations <- RpcClient
        .setup[IO](
          Stream.fromQueueNoneTerminated(inQueue),
          Services.empty,
          maxConcurrentServiceWorkers = 4,
        )
        .use { client =>
          for
            outQueue <- Queue.bounded[IO, Message](1)
            fiber <- client
              .messageStream
              .foreach(outQueue.offer)
              .compile
              .drain
              .start
            expectations <- Stream.emits(testActions).zip(Stream.range(0, testActions.size)).evalMap {
              case (action: ExpectNotification, id) =>
                for
                  _ <- client.notify(action.endpoint, action.notificationWithId(id))
                  clientMessage <- outQueue.take
                yield
                  clientMessage match
                    case Notification(method, params, _, _) =>
                      expect.same(method, action.endpoint.method) and
                      expect.same(
                        readFromArray(params.fold(Array.empty[Byte])(_.value))(using action.endpoint.codecA),
                        action.notificationWithId(id),
                      )
                    case msg => failure(s"found expected Notification, got: $msg")

              case (action: ExpectRequest, id) =>
                for
                  requestFiber <- client.request(action.endpoint, action.requestWithId(id)).start
                  clientMessage <- outQueue.take
                  _ <- clientMessage match
                    case msg: Request => inQueue.offer(Some(
                      Response.ok(RawJson.toJson(action.response)(using action.endpoint.codecB), msg.id)
                    ))
                    case msg => IO.raiseError(new Exception(s"expected client to send request, got: $msg"))
                  response <- requestFiber.join.flatMap {
                    case Outcome.Succeeded(response) => response.flatMap {
                      case RpcSuccess(value, _) => value.pure
                      case RpcFailure(_, response) =>
                        IO.raiseError(new Exception(s"recieved unexpected failure response: $response"))
                    }
                    case failedResponse => IO.raiseError(new Exception(s"client failed to produce response: $failedResponse"))
                  }
                yield
                  clientMessage match
                    case Request(method, params, _, _, _) =>
                      expect.same(method, action.endpoint.method) and
                      expect.same(
                        readFromArray(params.fold(Array.empty[Byte])(_.value))(using action.endpoint.codecA),
                        action.requestWithId(id),
                      )
                      expect.same(response, action.response)
                    case msg => failure(s"found expected Request, got: $msg")
            }.compile.toList
            _ <- inQueue.offer(None)
          yield expectations.fold(success)(_ and _)
        }
    yield expectations
  }

  def runTestActionsParallel(
    testActions: Array[TestAction],
  )(using CatsLogger[IO]): IO[Expectations] = {
    for
      notificationAcks <- Ref[IO].of(Map.empty[Int, Deferred[IO, Unit]])
      serverToClientQueue <- Queue.bounded[IO, Option[Message]](testActions.size)
      clientToServerQueue <- Queue.bounded[IO, Option[Message]](testActions.size)
      serverExpectations <- Ref[IO].of(List.empty[Expectations])
      serverServices = testActions.groupBy(_.endpoint.method).map { (endpoint, actions) =>
        actions(0) match
          case action: ExpectNotification =>
            Service.notification(action.endpoint) { notification =>
              testActions(notification.id) match
                case action: ExpectNotification =>
                  notificationAcks
                    .getAndUpdate(_ - notification.id)
                    .flatMap(_.apply(notification.id).complete(())) *>
                    serverExpectations.update(success +: _)
                case action =>
                  val msg = s"expected ExpectNotification test action for id ${notification.id}, got: $action"
                  serverExpectations.update(failure(msg) +: _) *> IO.raiseError(new Exception(msg))
            }
          case action: ExpectRequest =>
            Service.request(action.endpoint) { request =>
              testActions(request.id) match
                case matchedAction: ExpectRequest =>
                  serverExpectations.update(success +: _).as(
                    matchedAction.response.asInstanceOf[action.B])
                case action =>
                  val msg = s"expected ExpectRequest test action for id ${request.id}, got: $action"
                  serverExpectations.update(failure(msg) +: _) *> IO.raiseError(new Exception(msg))
            }
      }.foldLeft(Services.empty[IO])(_ addService _)
      clientExpectations <- (
        RpcClient.setup[IO](
          Stream.fromQueueNoneTerminated(serverToClientQueue),
          Services.empty,
          maxConcurrentServiceWorkers = 4,
        ),
        RpcClient.setup[IO](
          Stream.fromQueueNoneTerminated(clientToServerQueue),
          serverServices,
          maxConcurrentServiceWorkers = 4,
        )
      ).tupled.use { (client, server) =>
        for
          _ <- client
            .messageStream
            .foreach(msg => clientToServerQueue.offer(Some(msg)))
            .compile
            .drain
            .start
          _ <- server
            .messageStream
            .foreach(msg => serverToClientQueue.offer(Some(msg)))
            .compile
            .drain
            .start
          expectations <- Stream
            .emits(testActions)
            .zip(Stream.range(0, testActions.size))
            .parEvalMap(testActions.size) {
              case (action: ExpectNotification, id) =>
                for
                  deferred <- Deferred[IO, Unit]
                  _ <- notificationAcks.update(_ + (id -> deferred))
                  _ <- client.notify(action.endpoint, action.notificationWithId(id))
                  _ <- deferred.get
                yield success

              case (action: ExpectRequest, id) =>
                for
                  response <- client.request(action.endpoint, action.requestWithId(id)).flatMap {
                    case RpcSuccess(value, _) => value.pure
                    case RpcFailure(_, response) =>
                      IO.raiseError(new Exception(s"recieved unexpected failure response: $response"))
                  }
                yield expect(response == action.response)
            }.compile.toList
        yield expectations
      }
      serverExpectations <- serverExpectations.get
    yield
      expect(serverExpectations.size == testActions.size) and
      expect(clientExpectations.size == testActions.size) and
      (serverExpectations ++ clientExpectations).fold(success)(_ and _)
  }

  /** client            server
    *    --> openConnection(123)
    *    <-- 456
    *
    *    --> compile("foobar", 999)
    *    <-- (true, ["foo", "bar", "999"])
    *
    *    --> warn("I'm warning you.")
    *
    */
  loggedTest("test simple sequential") { log =>
    given CatsLogger[IO] = weaverLogToCatsLogger(log)
    runTestActions(Array(
      ExpectRequest(openConnection, OpenConnectionParams(123), OpenConnectionReturn(456)),
      ExpectRequest(
        compile,
        CompileParams("foobar", 999),
        CompileReturn(true, List("foo", "bar", "999")),
      ),
      ExpectNotification(warn, WarnParams("I'm warning you"))
    ))
  }

  loggedTest("test simple parallel") { log =>
    given CatsLogger[IO] = weaverLogToCatsLogger(log)
    runTestActionsParallel(Array(
      ExpectRequest(openConnection, OpenConnectionParams(123), OpenConnectionReturn(456)),
      ExpectRequest(
        compile,
        CompileParams("foobar", 999),
        CompileReturn(true, List("foo", "bar", "999")),
      ),
      ExpectNotification(warn, WarnParams("I'm warning you")),
      ExpectRequest(
        compile,
        CompileParams("foobar", 999),
        CompileReturn(true, List("foo", "bar", "999")),
      ),
      ExpectNotification(warn, WarnParams("I'm warning youu")),
      ExpectRequest(openConnection, OpenConnectionParams(123), OpenConnectionReturn(456)),
      ExpectRequest(
        compile,
        CompileParams("foobar", 999),
        CompileReturn(true, List("foo", "bar", "999")),
      ),
      ExpectNotification(warn, WarnParams("I'm warning you")),
      ExpectRequest(
        compile,
        CompileParams("foobar", 999),
        CompileReturn(true, List("foo", "bar", "999")),
      ),
      ExpectNotification(warn, WarnParams("I'm warning youu")),
    ))
  }

  inline given [T]: Show[T] = compiletime.summonFrom {
    case show: Show[T] => show
    case _ => new Show[T] {
      def show(t: T): String = t.toString
    }
  }

  import GenDerivers.given
  val testCaseGenerator = Arbitrary(
    Gen.nonEmptyBuildableOf[Array[TestAction], TestAction](
      Gen.oneOf(
        genExpectRequest(openConnection),
        genExpectRequest(compile),
        genExpectNotification(warn),
      ).flatMap(identity)
    )
  )

  loggedTest("test random sequential") { log =>
    given CatsLogger[IO] = weaverLogToCatsLogger(log)

    given Arbitrary[Array[TestAction]] = testCaseGenerator

    forall { (actions: Array[TestAction]) =>
      runTestActions(actions)
    }
  }

  loggedTest("test random parallel") { log =>
    given CatsLogger[IO] = weaverLogToCatsLogger(log)

    given Arbitrary[Array[TestAction]] = testCaseGenerator

    forall { (actions: Array[TestAction]) =>
      runTestActionsParallel(actions.toArray)
    }
  }

object Macros:
  import scala.quoted.{Quotes, Expr}
  inline def show(inline expr: Any): String =
    ${ showImpl('expr) }

  def showImpl(expr: Expr[Any])(using Quotes): Expr[String] = 
    Expr(expr.show)
