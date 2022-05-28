package buildsonnet.evaluator

import buildsonnet.ast.{HasSource, Source}
import cats.effect.Sync
import cats.syntax.all.given
import scala.collection.mutable

sealed trait EvaluatedJValue[F[_]] extends HasSource:
  import EvaluatedJValue._
  def isNull: Boolean = false

object EvaluatedJValue:
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

    def lookup(src: Source, field: String): F[LazyObjectValue[F]]

    def lookupOpt(src: Source, field: String): Option[LazyObjectValue[F]]

    def members: collection.Map[String, LazyObjectValue[F]]

  final class JObjectImpl[F[_]](
    val members: F[collection.Map[String, LazyObjectValue[F]]],
    val asserts: F[Unit],
  )

  object JObject:
    private class JObjectLeaf[F[_]: Sync](
      val src: Source,
      var ctx: EvaluationContext[F],
      var members: collection.Map[String, LazyObjectValue[F]],
      implFn: EvaluationContext[F] => JObjectImpl[F]
    ) extends JObject[F]:
      final def mixin(src: Source, child: JObject[F]): F[JObject[F]] =
        JObject.mixin(src, this, child).widen

      def withContext(newCtx: EvaluationContext[F]): F[(JObjectLeaf[F], JObjectImpl[F])] =
        Sync[F].defer {
          val obj = JObjectLeaf[F](src, null, null, implFn)
          obj.ctx = newCtx.withSelf(obj)
          val impl = implFn(obj.ctx)
          impl.members.map { members =>
            obj.members = members
            (obj, impl)
          }
        }

      def lookup(src: Source, field: String): F[LazyObjectValue[F]] =
         members
          .get(field)
          .fold(ctx.error(src, s"object missing field $field")) { value =>
            value.pure
          }

      def lookupOpt(src: Source, field: String): Option[LazyObjectValue[F]] =
        members.get(field)


    private class JObjectChain[F[_]: Sync](
      val src: Source,
      var leaves: Array[JObjectLeaf[F]]
    ) extends JObject[F]:
      final def mixin(src: Source, child: JObject[F]): F[JObject[F]] =
        JObject.mixin(src, this, child).widen

      def lookupOpt(src: Source, field: String): Option[LazyObjectValue[F]] =
        var result = Option.empty[LazyObjectValue[F]]
        var i = leaves.size - 1
        while result.isEmpty && i >= 0 do
          result = leaves(i).lookupOpt(src, field)
          i -= 1
        return result

      def lookup(src: Source, field: String): F[LazyObjectValue[F]] =
        lookupOpt(src, field)
          .fold(leaves.last.ctx.error(src, s"object missing field $field")) { value =>
            value.pure
          }

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
        parentPair <- parent.withContext(parent.ctx.withSelf(obj))
        (parent, parentImpl) = parentPair
        childAsserts <- Range(0, children.size).foldLeft(Sync[F].delay { obj.leaves(0) = parent; ().pure }) { (prevAsserts, i) =>
          val child = children(i)
          for
            prevAsserts <- prevAsserts
            childPair <- child.withContext(child.ctx.withSuper(obj.leaves(i)))
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
      ctx: EvaluationContext[F],
      implFn: EvaluationContext[F] => JObjectImpl[F],
    ): F[JObject[F]] =
      for
        obj <- Sync[F].defer {
          val obj = JObjectLeaf[F](src, null, null, implFn)
          obj.ctx = ctx.withSelf(obj)
          val impl = implFn(obj.ctx)
          impl.members.flatMap { members =>
            obj.members = members
            impl.asserts.as(obj)
          }
        }
      yield
        obj

    def static[F[_]: Sync](
      src: Source,
      ctx: EvaluationContext[F],
      members: Map[String, EvaluatedJValue[F]],
    ): JObject[F] =
      val lazyMembers = members.map { (name, value) =>
        name -> LazyObjectValue.strict(true, value)
      }
      val impl = JObjectImpl[F](
        lazyMembers.pure,
        ().pure,
      )
      val obj = JObjectLeaf[F](src, null, null, _ => impl)
      obj.ctx = ctx.withSelf(obj)
      obj.members = lazyMembers
      obj
