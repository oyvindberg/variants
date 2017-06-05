package variants

import utest._

import scala.meta._

object FunctorTest extends TestSuite {

  val tests = this {
    "ADT with one type param" {

      val before = TestUtils.parseTrait(
        """
@FunctorAnn
trait Adt[U] {

  sealed trait A[T]
  @Include("Adt1")
  sealed trait AA[T]  extends A[T]
  sealed trait AAA[T] extends A[T]
  case class B[T](d1: T, d2: T, u: U) extends AA[T] @Include("Adt1") with AAA[T] @Include("Adt2")

  @Include("Adt1")
  class C[T](val i: Int, val bs: Seq[B[T]]) extends A[T]

  case class D[T](@Include("Adt1") i: Int, @Exclude("Adt2") j: Int) extends A[T]

  case class E[T](@Include("Adt1") ob: Option[B[T]], @Include("Adt1") as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]
  case class F[T]() extends AAA[T]
}""")

      val actual: Stat = Gen(before, Seq("Adt1")).head

      val expected = TestUtils.parseTrait(
        """trait Adt1[U] {
  sealed trait A[T]
  sealed trait AA[T] extends A[T]
  sealed trait AAA[T] extends A[T]
  case class B[T](d1: T, d2: T, u: U) extends AA[T]
  class C[T](val i: Int, val bs: Seq[B[T]]) extends A[T]
  case class D[T](i: Int, j: Int) extends A[T]
  case class E[T](ob: Option[B[T]], as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]
  case class F[T]() extends AAA[T]
  class Adt1Functors(implicit SeqFunctor: Functor[Seq], OptionFunctor: Functor[Option]) {
    implicit lazy val AFunctor: Functor[A] = new Functor[A] {
      def map[T, TT](x: A[T])(f: T => TT): A[TT] = x match {
        case x: AA[T] =>
          AAFunctor.map(x)(f)
        case x: AAA[T] =>
          AAAFunctor.map(x)(f)
        case x: C[T] =>
          CFunctor.map(x)(f)
        case x: D[T] =>
          DFunctor.map(x)(f)
        case x: E[T] =>
          EFunctor.map(x)(f)
      }
    }
    implicit lazy val AAFunctor: Functor[AA] = new Functor[AA] {
      def map[T, TT](x: AA[T])(f: T => TT): AA[TT] = x match {
        case x: B[T] =>
          BFunctor.map(x)(f)
      }
    }
    implicit lazy val AAAFunctor: Functor[AAA] = new Functor[AAA] {
      def map[T, TT](x: AAA[T])(f: T => TT): AAA[TT] = x match {
        case x: F[T] =>
          FFunctor.map(x)(f)
      }
    }
    implicit lazy val BFunctor: Functor[B] = new Functor[B] { def map[T, TT](x: B[T])(f: T => TT): B[TT] = new B(d1 = f(x.d1), d2 = f(x.d2), u = x.u) }
    implicit lazy val CFunctor: Functor[C] = new Functor[C] { def map[T, TT](x: C[T])(f: T => TT): C[TT] = new C(i = x.i, bs = SeqFunctor.map(x.bs)(x => BFunctor.map(x)(x => f(x)))) }
    implicit lazy val DFunctor: Functor[D] = new Functor[D] { def map[T, TT](x: D[T])(f: T => TT): D[TT] = new D(i = x.i, j = x.j) }
    implicit lazy val EFunctor: Functor[E] = new Functor[E] { def map[T, TT](x: E[T])(f: T => TT): E[TT] = new E(ob = OptionFunctor.map(x.ob)(x => BFunctor.map(x)(x => f(x))), as = SeqFunctor.map(x.as)(x => AFunctor.map(x)(x => f(x))), d = DFunctor.map(x.d)(x => f(x)), oe = OptionFunctor.map(x.oe)(x => EFunctor.map(x)(x => f(x)))) }
    implicit lazy val FFunctor: Functor[F] = new Functor[F] { def map[T, TT](x: F[T])(f: T => TT): F[TT] = new F() }
  }
}
""")

      TestUtils.structurallyEqual(actual, expected)
    }


    "ADT without type param should fail" {

      val before = TestUtils.parseTrait(
        """
@FunctorAnn
trait Adt[U] {
  sealed trait A
  class C(val i: Int, val bs: Seq[B]) extends A
}""")
      intercept[java.lang.RuntimeException](Gen(before, Seq("Adt1"))) match {
        case re: RuntimeException => assert(re.getMessage == "Error: Only one type parameter is supported in order to generate a Functor. A has 0 at Position.None")
      }
      1
    }
  }
}
