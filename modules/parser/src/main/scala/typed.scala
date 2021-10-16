package root

import monix.eval.Task
import concurrent.{ExecutionContext, Future}
import cats.syntax.all._
import cats.data.Nested


opaque type TypedJValue[T] = EvaluatedJValue.JError | T

type TypedJValueTask[T] = Task[TypedJValue[T]]

object TypedJValueTask:
  import TypedJValue._

  def nested[T](t: Task[TypedJValue[T]]): Nested[Task, TypedJValue, T] = Nested(t)

  def apply[T](t: Task[TypedJValue[T]]) = t

  extension [T](value: TypedJValueTask[T])
    def toTask: Task[TypedJValue[T]] = value

  given cats.Monad[TypedJValueTask] with
    def flatMap[A, B](fa: TypedJValueTask[A])(f: A => TypedJValueTask[B]): TypedJValueTask[B] =
      fa.flatMap { typed =>
        typed.toEither.fold(
          e => Task.now(TypedJValue.error[B](e)),
          f,
        )
      }

    def pure[A](a: A): TypedJValue[A] = a

    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: (A) => TypedJValue[Either[A, B]]): TypedJValue[B] =
      f(a) match
      case Left(a) => tailRecM(a)(f)
      case Right(b) => b


object TypedJValue:
  import cats.syntax.functor.given

  extension [T <: EvaluatedJValue](value: TypedJValue[T])
    def toEvaluated: EvaluatedJValue = value

  extension [T](value: TypedJValue[T])
    def fold[A](
      errorCase: EvaluatedJValue.JError => A,
      typedCase: T => A,
    ): A =
      value match
      case t: T => typedCase(t)
      case e: EvaluatedJValue.JError => errorCase(e)

    def toEither: Either[EvaluatedJValue.JError, T] =
      value match
      case t: T => Right(t)
      case e: EvaluatedJValue.JError => Left(e)

  given cats.Monad[TypedJValue] with
    def flatMap[A, B](fa: TypedJValue[A])(f: A => TypedJValue[B]): TypedJValue[B] =
      fa match
      case a: A => f(a)
      case e: EvaluatedJValue.JError => e

    def pure[A](a: A): TypedJValue[A] = a

    @scala.annotation.tailrec
    def tailRecM[A, B](a: A)(f: (A) => TypedJValue[Either[A, B]]): TypedJValue[B] =
      f(a) match
      case Left(a) => tailRecM(a)(f)
      case Right(b) => b

  def apply[T](t: T)(using scala.util.NotGiven[T =:= EvaluatedJValue.JError]): TypedJValue[T] = t
  def error[T](e: EvaluatedJValue.JError): TypedJValue[T] = e

