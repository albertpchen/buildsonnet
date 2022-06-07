package jsonrpc4cats

import cats.Show
import org.scalacheck.{Arbitrary, Gen}
import scala.deriving.Mirror
import scala.compiletime

object GenDerivers:
  given Gen[String] = Arbitrary.arbString.arbitrary
  given Gen[Int] = Arbitrary.arbInt.arbitrary
  given Gen[Boolean] = Arbitrary.arbBool.arbitrary
  given [T](using gen: Gen[T]): Gen[List[T]] = Gen.listOf(gen)

  inline given deriveGen[T](using m: Mirror.Of[T]): Gen[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        val gens = compiletime.summonAll[s.MirroredElemTypes].toArray.asInstanceOf[Array[Gen[T]]]
        Gen.oneOf(gens).flatMap(identity)
      case p: Mirror.ProductOf[T] =>
        deriveGenProduct[p.MirroredElemTypes].map { product =>
          p.fromProduct(product)
        }

  inline def deriveGenProduct[T <: Tuple]: Gen[T] =
    inline compiletime.erasedValue[T] match
      case _: EmptyTuple => Gen.const(EmptyTuple).asInstanceOf[Gen[T]]
      case _: (head *: tail) => compiletime.summonInline[Gen[head]].flatMap { head =>
        deriveGenProduct[tail].map(head *: _).asInstanceOf[Gen[T]]
      }
