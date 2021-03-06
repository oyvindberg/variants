package tester

import variants.{Functor, Exclude, FunctorAnn, Include, NewScope, Variants, Transformer}

object Dinos {
  @Variants("One", "Two") @FunctorAnn
  trait Base {
    trait Animal[+T]
    @Include("Two") trait LivingAnimal[+T] extends Animal[T]
    case class Rhino[T](@Exclude("One") weight: Int, friends: Seq[Animal[T]], @Include("Two") secrets: Option[Seq[T]]) extends LivingAnimal[T] @Include("Two") with Animal[T] @Include("One")
    case class Dino[T](height: Int, @Include("Two") enemy: Option[Animal[T]]) extends Animal[T]
    @Include("Two")
    case object Dodo extends Animal[Nothing]
  }
}

object Tester extends App {
  @Transformer
  @FunctorAnn
  @Variants("Adt1", "Adt2")
  trait Adt[U] {

    sealed trait A[+T]
    @Include("Adt1")
    sealed trait AA[+T]  extends A[T]
    sealed trait AAA[+T] extends A[T]
    case class B[T](d1: T, d2: T, u: U) extends AA[T] @Include("Adt1") with AAA[T] @Include("Adt2")

    @Include("Adt1")
    class C[T](val i: Int, val bs: Seq[B[T]]) extends A[T]

    case class D[T](@Include("Adt1") i: Int, @Exclude("Adt2") j: Int) extends A[T]

    case class E[T](@Include("Adt1") ob: Option[B[T]], @Include("Adt1") as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]
    case object F extends AAA[Nothing]
  }

  case class XX(i: Int)
  object Adt1 extends Adt1[XX]
  import Adt1._

  val e: E[Double] = E(Some(B(2.2, 4.4, XX(22))), Seq(F, D(42, 21), new C(9, Seq(B(1.0, 2.0, XX(99))))), D(2, 5), None)
  val ee = e.copy(oe = Some(e))

  case class V[T](implicit N: Numeric[T], newScope: NewScope[List[A[T]], A[T]]) extends Adt1Transformer[List[A[T]], T] {
    override def enterD(scope: List[A[T]])(_0: D[T]): D[T] = {
      val _1: D[T] = _0.copy(_0.i * 2)
      _1
    }

    override def enterB(scope: List[A[T]])(_0: B[T]): B[T] = {
      val _1 = B(N.times(_0.d1, N.fromInt(10)), _0.d2, XX(99))
      println(s"Rewriting ${_0} => ${_1} in scope ${scope map (_.getClass.getSimpleName) mkString " => "}")
      _1
    }
  }

  val eee = V[Double]().visitE(Nil)(ee)

  import Functor.FunctorOps
  object Functors extends Adt1Functors
  import Functors._

  val eeee = eee.map(d => s"MY double was $d")
  println(eeee)
}
