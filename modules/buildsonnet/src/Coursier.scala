package buildsonnet

import buildsonnet.evaluator.JDecoder

import coursier.{Dependency, Fetch, Module, ModuleName, Organization}
import cats.instances.all.given

case class CoursierParams(
  deps: List[CoursierDependency]
)

object CoursierParams:
  given [F[_]: cats.Monad: cats.Parallel]: JDecoder[F, CoursierParams] = JDecoder.derived

case class CoursierDependency(
  organization: String,
  name: String,
  version: String,
):
  def toDependency: Dependency =
    Dependency(Module(Organization(organization), ModuleName(name)), version)

object CoursierDependency:
  given [F[_]: cats.Monad]: JDecoder[F, CoursierDependency] = JDecoder.derived

import cats.syntax.all.given
import coursier.util.{Gather, Monad, Sync}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import java.util.concurrent.ExecutorService

object CoursierCatsInterop:
  given [F[_]](using M: cats.Monad[F]): Monad[F] with
    def point[A](a: A)                       = M.pure(a)
    def bind[A, B](elem: F[A])(f: A => F[B]) = M.flatMap(elem)(f)

  given [F[_], F0[_]](using N: cats.Monad[F], cs: cats.Parallel.Aux[F, F0]): Gather[F] with
    def point[A](a: A)                       = N.pure(a)
    def bind[A, B](elem: F[A])(f: A => F[B]) = N.flatMap(elem)(f)
    def gather[A](elems: Seq[F[A]]) =
        N.map(_root_.cats.Parallel.parSequence(elems.toVector))(_.toSeq)

  given [F[_]](using S: cats.effect.Async[F], G: Gather[F]): Sync[F] with
    def delay[A](a: => A): F[A] = S.delay(a)
    def fromAttempt[A](a: Either[Throwable, A]): F[A] = S.fromEither(a)
    def gather[A](elems: Seq[F[A]]): F[Seq[A]] = G.gather(elems)
    def handle[A](a: F[A])(f: PartialFunction[Throwable, A]): F[A] = a.recover(f)
    def point[A](a: A): F[A] = G.point(a)
    def schedule[A](pool: ExecutorService)(f: => A): F[A] = {
      val ec0 = pool match
        case eces: ExecutionContextExecutorService => eces
        // FIXME Is this instantiation costly? Cache it?
        case _ => ExecutionContext.fromExecutorService(pool)
      S.evalOn(S.delay(f), ec0)
    }
    def bind[A, B](elem: F[A])(f: A => F[B]) = S.flatMap(elem)(f)
