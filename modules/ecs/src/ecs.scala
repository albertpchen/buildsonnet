package ecs

import collection.mutable.ArrayBuffer

/** API
 *
 *  final class JObject extends Node:
 *    def eval()
 *
 *  val context = Context()
 *    .withEntity(entityType)
 *    .withEntity(entityType)
 *    .withEntity(entityType)
 *    .withEntity(entityType)
 */


final class Component[Name, Value] private()

sealed trait DefaultValue[T]:
  def value: T

object DefaultValue:
  private def apply[T](t: T): DefaultValue[T] =
    new DefaultValue[T]:
      def value = t

  given DefaultValue[String] = DefaultValue("")
  given DefaultValue[Int] = DefaultValue(0)
  given DefaultValue[Long] = DefaultValue(0)
  given DefaultValue[Double] = DefaultValue(0)
  given DefaultValue[Boolean] = DefaultValue(false)

import scala.language.dynamics
object Component extends Dynamic:
  def selectDynamic[V: DefaultValue](name: String): Component[name.type, V] = Component()


opaque type Id = Int
object Id:
  private[ecs] def apply(id: Int): Id = id

  extension(id: Id)
    private[ecs] def toInt: Int = id


opaque type System[Components] = Array[ArrayBuffer[?]]

object System:
  inline def apply[Components <: Tuple](tuple: Components): System[Components] =
    inline compiletime.erasedValue[Components] match
      case _: EmptyTuple =>
        compiletime.error("cannot construct System for an empty tuple")
      case _ =>
        val system = Array.ofDim[ArrayBuffer[?]](compiletime.constValue[Tuple.Size[Components]])
        initSystem[Components](system)
        system

  inline def initSystem[Components](
    system: System[Components],
    idx: Int = 0,
  ): Unit =
    inline compiletime.erasedValue[Components] match
    case _: (Component[_, value] *: tail) =>
      system(idx) = ArrayBuffer[value]()
      initSystem[tail](system, idx + 1)
    case _: EmptyTuple =>

  extension[T](system: System[T])
    inline def indexOf[Name, Value](c: Component[Name, Value], start: Int = 0): Int =
      inline compiletime.erasedValue[T] match
      case _: (Component[Name, Value] *: tail) => start
      case _: (_ *: tail) => indexOf(c, start + 1)
      case _: EmptyTuple => compiletime.error("system does not contain component")

    inline def apply[Name, Value](id: Id, component: Component[Name, Value]): Value =
      system(indexOf(component))(id).asInstanceOf[Value]

    inline def foreachComponentsAt[Components](id: Id)(fn: [A] => A => Unit, typeIdx: Int = 0): Unit =
      inline compiletime.erasedValue[Components] match
      case _: (Component[_, value] *: tail) =>
        fn[value](system(typeIdx).asInstanceOf[ArrayBuffer[value]](id.toInt))
        foreachComponentsAt[tail](id)(fn, typeIdx + 1)
      case _: EmptyTuple =>

    inline def mapAtType[Components, Name, Value](id: Id, fn: Value => Value): Unit =
      inline compiletime.erasedValue[Components] match
      case _: (Component[Name, Value] *: tail) =>
        val arr = system(id.toInt).asInstanceOf[ArrayBuffer[Value]]
        arr(id.toInt) = fn(arr(id.toInt))
      case _: (_ *: tail) =>
        mapAtType[tail, Name, Value](id, fn)
      case _: EmptyTuple =>
        compiletime.error("system does not contain component")

    inline def increaseByOne[Components](idx: Int): Unit =
      inline compiletime.erasedValue[Components] match
      case _: (Component[name, value] *: tail) =>
        system(idx).asInstanceOf[ArrayBuffer[value]] += compiletime.summonInline[DefaultValue[value]].value
        increaseByOne[tail](idx + 1)
      case _: EmptyTuple =>

    inline def get[Components, Name, Value](id: Id, c: Component[Name, Value], idx: Int = 0): Unit =
      inline compiletime.erasedValue[Components] match
      case _: (Component[Name, Value] *: tail) =>
        system(idx).asInstanceOf[ArrayBuffer[Value]](id.toInt)
      case _: (_ *: tail) =>
        get[tail, Name, Value](id, c, idx + 1)
      case _: EmptyTuple =>
        compiletime.error("system does not contain component")

    inline def set[Name, Value](id: Id, c: Component[Name, Value], value: Value): Unit =
      setImp[T, Name, Value](id, value, 0)

    inline def setImp[Components, Name, Value](id: Id, value: Value, idx: Int = 0): Unit =
      inline compiletime.erasedValue[Components] match
      case _: (Component[Name, Value] *: tail) =>
        system(idx).asInstanceOf[ArrayBuffer[Value]](id.toInt) = value
      case _: (_ *: tail) =>
        setImp[tail, Name, Value](id, value, idx + 1)
      case _: EmptyTuple =>
        compiletime.error("system does not contain component")

    inline def newEntity: Id =
      val newId = Id(system.head.size)
      increaseByOne[T](0)
      newId
      
    inline def prettyPrintImp(builder: StringBuilder): Unit =
      foreachComponentsAt[T](0) { [T] => (t: T) =>
        builder ++= t.toString
        builder += ','
        ()
      }
      for i <- 1 until system.head.size do
        builder += '\n'
        foreachComponentsAt[T](i) { [T] => (t: T) =>
          builder ++= t.toString
          builder += ','
          ()
        }

    inline def prettyPrint: String =
      if system.head.isEmpty then
        "<empty system>"
      else
        val builder = new StringBuilder
        prettyPrintImp(builder)
        builder.toString

@main
def test: Unit =
  val name = Component.name[String]
  val idx = Component.idx[Int]
  val system = System((name, idx))
  println(system.prettyPrint + '\n')
  val id1 = system.newEntity
  val id2 = system.newEntity
  println(system.prettyPrint + '\n')

  system.set(id1, name, "KJLKJ")
  system.set(id1, idx, 1234)

  system.set(id2, name, "asfdasdfj")
  system.set(id2, idx, 4321)

  println(system.prettyPrint + '\n')


sealed trait Box[+A, E]

case class Pure[A](value: A) extends Box[A, Nothing]
case class Lazy[A, E](thunk: () => Either[E, A]) extends Box[A, E]
sealed abstract class FlatMap[A, E] extends Box[A, E]:
  type InnerValue
  val fa: Box[InnerValue, E]
  val fn: InnerValue => Box[A, E]

case class Error[E](e: E) extends Box[Nothing, E]

object Box:
  def flatMap[A, B, E](fa: Box[A, E])(fn: A => Box[B, E]): Box[B, E] =
    fa match
      case Pure(value) => fn(value)
      case Lazy(thunk) => Lazy { () =>
        thunk() match
        case Left(e) => Left(e)
        case Right(a) => fn(a) match
          case Pure(value) => Right(value)
          case Lazy(thunk) => thunk()
          case Error(e) => Left(e)
      }
      case Error(e) => Error(e)

  def pure[A](a: A): Box[A, Nothing] =
    Pure(a)
