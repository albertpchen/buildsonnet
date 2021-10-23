package root

// type JType =
//     Boolean
//   | Double
//   | String
//   | Unit
//   | Seq[EvaluatedJValue]
//   | EvaluatedJValue.JObject
//   | EvaluatedJValue.JFunction

// type JType[L[_], A] = A match
//   case Int => Int
//   case _ =>
//       Boolean
//     | Double
//     | String
//     | Unit
//     | EvaluatedJValue.JObject
//     | EvaluatedJValue.JFunction
//     | L[JType[L, A]]


type JTypeOf[T <: EvaluatedJValue] = T match
  case EvaluatedJValue.JBoolean   => Boolean
  case EvaluatedJValue.JNull.type => Unit
  case EvaluatedJValue.JString    => String
  case EvaluatedJValue.JNum       => Double
  case EvaluatedJValue.JArray     => Seq[EvaluatedJValue]
  case EvaluatedJValue.JObject    => EvaluatedJValue.JObject
  case EvaluatedJValue.JFunction  => EvaluatedJValue.JFunction

type EvaluatedJTypeOf[T] = T match
  case Boolean                   => EvaluatedJValue.JBoolean
  case Double                    => EvaluatedJValue.JNull.type
  case String                    => EvaluatedJValue.JString
  case Unit                      => EvaluatedJValue.JNum
  case Seq[EvaluatedJValue]      => EvaluatedJValue.JArray
  case EvaluatedJValue.JObject   => EvaluatedJValue.JObject
  case EvaluatedJValue.JFunction => EvaluatedJValue.JFunction

import monix.eval.Task
import scala.concurrent.{ExecutionContext, Future}
import cats.syntax.all.given

enum RunningBox[+A]:
  case Now(value: A)
  case Later(value: Future[A])

  def zip[B](fb: RunningBox[B])(using ExecutionContext): RunningBox[(A, B)] =
    (this, fb) match
    case (Now(a), Now(b)) => Now(a -> b)
    case (Later(a), Later(b)) => Later(a.zip(b))
    case _ => this.flatMap(a => fb.map(a -> _))

  def map[B](fn: A => B)(using ExecutionContext): RunningBox[B] =
    this match
    case Now(value) => Now(fn(value))
    case Later(future) => Later(future.map(fn))

  def flatMap[B](fn: A => RunningBox[B])(using ExecutionContext): RunningBox[B] =
    this match
    case Now(value) => fn(value)
    case Later(future) => Later(future.map(fn(_).await))

  def await(using ExecutionContext): A =
    this match
    case Now(value) => value
    case Later(future) => concurrent.Await.result(future, concurrent.duration.Duration.Inf)

sealed trait Box[+A]:
  def eval(using ExecutionContext): RunningBox[A]

case class Pure[A](value: A) extends Box[A]:
  def eval(using ExecutionContext): RunningBox[A] = RunningBox.Now(value)

case class Async[A](thunk: ExecutionContext => A) extends Box[A]:
  def eval(using ec: ExecutionContext): RunningBox[A] = RunningBox.Later(Future(thunk(ec)))


object Box:
  type Unwrap[T] <: Tuple = T match
    case (Box[x] *: tail) => x *: Unwrap[tail]
    case EmptyTuple => EmptyTuple

  type Invert[T] <: Box[Tuple] = T match
    case (Box[x] *: tail) => Box[x *: Unwrap[tail]]
    case EmptyTuple => Box[EmptyTuple]

  transparent inline def parZip[T](t: T): Invert[T] =
    inline t match
    case t: (Box[x] *: tail) =>
      parZip2(t.head, parZip[tail](t.tail)).map((a, b) => a *: b.asInstanceOf[Unwrap[tail]])
    case _: EmptyTuple => Pure(EmptyTuple)

  transparent inline def flatten[T](t: T): Tuple =
    inline t match
    case t: (head, tail) => t._1 *: flatten(t._2)
    case _: EmptyTuple => EmptyTuple

  def parZip3[A, B, C](a: Box[A], b: Box[B], c: Box[C]): Box[(A, B, C)] =
    parZip((a, b, c))

  def parZip2[A, B](a: Box[A], b: Box[B]): Box[(A, B)] =
    (a, b) match
    case (Pure(a), b) => b.map((a, _))
    case (a, Pure(b)) => a.map((_, b))
    case (a, b) => Async { implicit ec =>
      val ae = a.eval
      val be = b.eval
      ae.zip(be).await
    }

  def parSequence[A, L[X] <: Iterable[X]](
    in: L[Box[A]]
  )(using bf: collection.BuildFrom[L[Box[A]], A, L[A]]): Box[L[A]] =
    Async { implicit ec =>
      val l = in.map(_.eval).foldRight(RunningBox.Now(Seq.empty[A])) { case (box, acc) =>
        box.flatMap(a => acc.map(a +: _))
      }
      l.map(bf.fromSpecific(in)(_)).await
    }

  given cats.Monad[Box] with
    def flatMap[A, B](fa: Box[A])(fn: A => Box[B]): Box[B] =
      fa match
      case Pure(value) => fn(value)
      case Async(thunk) => Async { implicit ec =>
        fn(thunk(ec)) match
        case Pure(value) => value
        case Async(thunk) => thunk(ec)
      }

    def pure[A](a: A): Box[A] = Pure(a)

    def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] =
      Async { implicit ec =>
        var currA: A = a
        var bOpt: Option[B] = None
        while bOpt.isEmpty do
          f(a).eval.await.fold(
            a => currA = a,
            b => bOpt = Some(b)
          )
        bOpt.get
      }
