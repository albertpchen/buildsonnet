package buildsonnet.evaluator

import buildsonnet.ast.{HasSource, Source}
import cats.{Applicative, Monad, Parallel}
import cats.effect.Sync
import cats.syntax.all.given
import scala.collection.mutable

sealed trait EvaluatedJValue[F[_]] extends HasSource:
  import EvaluatedJValue._
  def isNull: Boolean = false

object EvaluatedJValue:
  extension [F[_]: Monad: Parallel](value: EvaluatedJValue[F])
    def structuralEquals(other: EvaluatedJValue[F]): F[Boolean] =
      (value, other) match
      case (op1: JBoolean[F], op2: JBoolean[F]) => (op1.bool == op2.bool).pure
      case (op1: JNull[F], op2: JNull[F]) => true.pure
      case (op1: JString[F], op2: JString[F]) => (op1.string == op2.string).pure
      case (op1: JNum[F], op2: JNum[F]) => (op1.double == op2.double).pure
      case (op1: JArray[F], op2: JArray[F]) =>
        if op1.elements.size == op2.elements.size then
          op1.elements.zip(op2.elements).foldLeft(true.pure) {
            case (result, (op1, op2)) => result.flatMap { res =>
              if res then
                op1.structuralEquals(op2)
              else
                result
            }
          }
        else
          false.pure
      case (op1: JFunction[F], op2: JFunction[F]) => (op1 eq op2).pure
      case (op1: JObject[F], op2: JObject[F]) =>
        val members1 = op1.members
        val members2 = op2.members
        if members1.keys == members2.keys then
          members1.keys.foldLeft(true.pure) {
            case (result, key) => result.flatMap { res =>
              if res then
                (members1(key).value, members2(key).value).parTupled.flatMap(_.structuralEquals(_))
              else
                result
            }
          }
        else false.pure
      case (op1, op2) => false.pure

  def escape(s: String, builder: StringBuilder): Unit =
    var idx = 0
    val len = s.length
    while idx < len do
      (s(idx): @annotation.switch) match
        case '"'  => builder ++= "\\\""
        case '\\' => builder ++= "\\\\"
        case '\b' => builder ++= "\\b"
        case '\f' => builder ++= "\\f"
        case '\n' => builder ++= "\\n"
        case '\r' => builder ++= "\\r"
        case '\t' => builder ++= "\\t"
        case c =>
          val shouldEscape = (c >= '\u0000' && c <= '\u001f')
          || (c >= '\u0080' && c < '\u00a0')
          || (c >= '\u2000' && c < '\u2100')
          if shouldEscape then
            builder ++= "\\u%04x".format(c: Int)
          else
            builder ++= c.toString
      idx += 1

  case class JBoolean[F[_]](src: Source, bool: Boolean) extends EvaluatedJValue[F]
  case class JNull[F[_]](src: Source) extends EvaluatedJValue[F] {
    override def isNull: Boolean = true
  }
  case class JString[F[_]](src: Source, string: String) extends EvaluatedJValue[F]
  case class JNum[F[_]](src: Source, double: Double) extends EvaluatedJValue[F]
  case class JArray[F[_]](src: Source, elements: IArray[EvaluatedJValue[F]]) extends EvaluatedJValue[F]

  case class JFunctionParameters[F[_]](
    src: Source,
    ctx: EvaluationContext[F],
    positionalArgs: Seq[EvaluatedJValue[F]],
    namedArgs: Seq[(String, EvaluatedJValue[F])],
  )
  case class JFunction[F[_]](
    src: Source,
    numParams: Int,
    fn: JFunctionParameters[F] => F[EvaluatedJValue[F]]) extends EvaluatedJValue[F]

  sealed trait JObject[F[_]] extends EvaluatedJValue[F]:
    def mixin(src: Source, child: JObject[F]): F[JObject[F]]

    def lookupOpt(field: String): Option[LazyObjectValue[F]]

    def members: collection.Map[String, LazyObjectValue[F]]

  final class JObjectImplParams[F[_]](
    val `super`: Option[JObject[F]],
    val `self`: JObject[F],
  )
  final class JObjectImpl[F[_]](
    val members: F[collection.Map[String, LazyObjectValue[F]]],
    val asserts: F[Unit],
  )

  object JObject:
    private class JObjectLeaf[F[_]: Sync](
      val src: Source,
      var `super`: Option[JObject[F]],
      var `self`: JObject[F],
      var members: collection.Map[String, LazyObjectValue[F]],
      implFn: JObjectImplParams[F] => JObjectImpl[F],
    ) extends JObject[F]:
      final def mixin(src: Source, child: JObject[F]): F[JObject[F]] =
        JObject.mixin(src, this, child).widen

      def withSelfSuper(newSuper: Option[JObject[F]], newSelf: JObject[F]): F[(JObjectLeaf[F], JObjectImpl[F])] =
        Sync[F].defer {
          val obj = JObjectLeaf[F](src, newSuper, newSelf, null, implFn)
          val impl = implFn(JObjectImplParams(newSuper, newSelf))
          impl.members.map { members =>
            obj.members = members
            (obj, impl)
          }
        }

      def withSelf(newSelf: JObject[F]): F[(JObjectLeaf[F], JObjectImpl[F])] =
        withSelfSuper(`super`, newSelf)

      def lookupOpt(field: String): Option[LazyObjectValue[F]] =
        members.get(field)


    private class JObjectChain[F[_]: Sync](
      val src: Source,
      var leaves: Array[JObjectLeaf[F]]
    ) extends JObject[F]:
      final def mixin(src: Source, child: JObject[F]): F[JObject[F]] =
        JObject.mixin(src, this, child).widen

      def lookupOpt(field: String): Option[LazyObjectValue[F]] =
        var result = Option.empty[LazyObjectValue[F]]
        var i = leaves.size - 1
        while result.isEmpty && i >= 0 do
          result = leaves(i).lookupOpt(field)
          i -= 1
        return result

      lazy val members: collection.Map[String, LazyObjectValue[F]] =
        leaves.foldLeft(mutable.HashMap.empty[String, LazyObjectValue[F]]) { (map, leaf) =>
          map ++= leaf.members
        }

    private def mixin[F[_]: Sync](src: Source, p: JObject[F], c: JObject[F]): F[JObjectChain[F]] =
      val (parent, children) = (p, c) match
        case (parent: JObjectLeaf[F], child: JObjectLeaf[F]) => (parent, Array(child))
        case (parent: JObjectLeaf[F], child: JObjectChain[F]) => (parent, child.leaves)
        case (parent: JObjectChain[F], child: JObjectLeaf[F]) => (parent.leaves(0), parent.leaves.drop(1) :+ child)
        case (parent: JObjectChain[F], child: JObjectChain[F]) => (parent.leaves(0), parent.leaves.drop(1) ++ child.leaves)
      for
        obj <- Sync[F].delay(new JObjectChain[F](src, Array.fill(children.size + 1)(null)))
        parentPair <- parent.withSelf(obj)
        (parent, parentImpl) = parentPair
        childAsserts <- Range(0, children.size).foldLeft(Sync[F].delay { obj.leaves(0) = parent; ().pure }) { (prevAsserts, i) =>
          val child = children(i)
          for
            prevAsserts <- prevAsserts
            childPair <- child.withSelfSuper(Some(obj.leaves(i)), obj)
          yield
            val (child, childImpl) = childPair
            obj.leaves(i + 1) = child
            prevAsserts *> childImpl.asserts
        }
        _ <- parentImpl.asserts *> childAsserts
      yield
        obj

    def apply[F[_]: Sync](
      src: Source,
      implFn: JObjectImplParams[F] => JObjectImpl[F],
    ): F[JObject[F]] =
      for
        obj <- Sync[F].defer {
          val obj = JObjectLeaf[F](src, None, null, null, implFn)
          obj.self = obj
          val impl = implFn(JObjectImplParams(None, obj))
          impl.members.flatMap { members =>
            obj.members = members
            impl.asserts.as(obj)
          }
        }
      yield
        obj

    def static[F[_]: Sync](
      src: Source,
      members: Map[String, EvaluatedJValue[F] | LazyObjectValue[F]],
    ): JObject[F] =
      val lazyMembers = members.map {
        case (name, value: EvaluatedJValue[F]) => name -> LazyObjectValue.strict(true, value)
        case (name, value: LazyObjectValue[F]) => name -> value
      }
      val impl = JObjectImpl[F](
        lazyMembers.pure,
        ().pure,
      )
      val obj = JObjectLeaf[F](src, None, null, null, _ => impl)
      obj.self = obj
      obj.members = lazyMembers
      obj
