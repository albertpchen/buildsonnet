package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.{Event, Element, Node, console, document}

import cats.effect.{Async, Deferred, IO, IOApp, ExitCode}
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.kernel.{Resource}
import cats.syntax.all.given
import fs2.Stream

/*
  for {
    dispatcher <- Stream.resource(Dispatcher[F])
    q <- Stream.eval(Queue.unbounded[F, Option[RowOrError]])
    _ <- Stream.eval { F.delay {
      def enqueue(v: Option[RowOrError]): Unit = dispatcher.unsafeRunAndForget(q.offer(v))

      // Fill the data - withRows blocks while reading the file, asynchronously invoking the callback we pass to it on every row
      h.withRows(e => enqueue(Some(e)))
      // Upon returning from withRows, signal that our stream has ended.
      enqueue(None)
    }}
    // Due to `fromQueueNoneTerminated`, the stream will terminate when it encounters a `None` value
    row <- Stream.fromQueueNoneTerminated(q).rethrow
  } yield row

   def button[F[_]](
    to: Element,
    caption: String
  )(implicit F: ConcurrentEffect[F]): Stream[F, Event] = {

    def addElement(q: Queue[F, Event]): F[Node] = F.delay {
      val newElement = input(
        `type` := "button",
        `value` := caption
      ).render
      newElement.addEventListener(
        `type` = "click",
        listener = (e: Event) => F.toIO(q.enqueue1(e)).unsafeRunAsyncAndForget
      )
      to.appendChild(newElement)
    }

    def removeElement(element: Node): F[Unit] =
      F.delay(to.removeChild(element))

    Stream.eval(Queue.circularBuffer[F, Event](maxSize = 10))
      .flatMap { queue =>
        Stream.bracket(addElement(queue))(removeElement)
          .flatMap(_ => queue.dequeue)
      }
  }
 */
/**
  * Add a button to the DOM and
  * set up a stream of click events from it
  */
def buttonAndClicked[F[_]](
  to: Element,
  caption: String
)(using F: Async[F]): F[(Element, Deferred[F, Event])] =
  Dispatcher[F].use { dispatcher =>
    for
      deferred <- Deferred[F, Event]
    yield
      val newElement = document.createElement("button")
      newElement.innerHTML = caption
      newElement.addEventListener(
        `type` = "click",
        listener = e => {
          to.removeChild(newElement)
          console.log("RMEOVE")
          dispatcher.unsafeRunAndForget(deferred.complete(e).map(_ => console.log("LLLL")))
        }
      )
      to.appendChild(newElement)
      newElement -> deferred
  }

def button[F[_]](
  to: Element,
  caption: String
)(using F: Async[F]): Stream[F, Event] =
  for
    dispatcher <- Stream.resource(Dispatcher[F])
    q <- Stream.eval(Queue.unbounded[F, Event])
    s <- Stream.bracket(F.delay {
      val newElement = document.createElement("button")
      newElement.innerHTML = caption
      newElement.addEventListener(
        `type` = "click",
        listener = e => dispatcher.unsafeRunAndForget(q.offer(e))
      )
      to.appendChild(newElement)
      newElement
    })(e => F.delay(to.removeChild(e)))
    // Due to `fromQueueNoneTerminated`, the stream will terminate when it encounters a `None` value
    //event <- s.evalMap(_.take)
    _ = s.innerHTML = "-" + s.innerHTML
    event <- Stream.fromQueueUnterminated(q)
  yield event
  // Stream.eval(
  //   Queue.circularBuffer[F, Event](capacity = 10).map { q =>
  //     F.delay {
  //       def enqueue(v: Option[RowOrError]): Unit = dispatcher.unsafeRunAndForget(q.offer(v))

  //       // Fill the data - withRows blocks while reading the file, asynchronously invoking the callback we pass to it on every row
  //       h.withRows(e => enqueue(Some(e)))
  //       // Upon returning from withRows, signal that our stream has ended.
  //       enqueue(None)
  //     }
  //     val newElement = document.createElement("input")
  //     newElement.addEventListener(
  //       `type` = "click",
  //       listener = (e: Event) => q.offer(e)
  //     )
  //     to.appendChild(newElement)
  //     q
  //   }
  // ).evalMap(_.take)

// @main
// def hello(): Unit =
//   val parNode = document.createElement("p")
//   parNode.textContent = "SDLFKJ"
//   document.body.appendChild(parNode)
//   println("Hello world!")

final class State[F[_]](
  var clicks: Int,
  var buttonAndClicked: Option[(Element, Deferred[F, Event])],
)

/*
 * 1. for each click of outer button
 * 2. if button already made return it, otherwise create a new one
 * 3. s
 */
object hello extends IOApp:
  def run(args: List[String]) =

    val body = document.body
    val initState = buttonAndClicked[IO](body, "clicked 1 time").map { buttonAndClicked =>
      State(1, Some(buttonAndClicked))
    }
    button[IO](body, "Click me").evalScan(State[IO](0, None)) { (state, _) =>
      if state.buttonAndClicked.isDefined then
        val (element, clicked) = state.buttonAndClicked.get
        for
          clickedOpt <- clicked.tryGet
          nextState <-
            if clickedOpt.isDefined then
              initState
            else
              IO {
                console.log("SDLKFJLJ")
                element.innerHTML = s"clicked ${state.clicks + 1} times"
                State(state.clicks + 1, Some((element, clicked)))
              }
        yield
          nextState
      else
        initState
    }
    .compile.drain.as(ExitCode.Success)
/*
    button[IO](body, "Click me")

      // keep track of how often it is clicked
      // and emit only that number downstream
      .zipWithIndex.map(_._2)

      // add a button that will be removed after one click.
      // using switchMap means this stream will be interrupted
      // if the first button is clicked, so the count button will update
      .switchMap(ts => button[IO](body, s"Clicked $ts times").take(1))
      .compile.drain.as(ExitCode.Success)
      */
