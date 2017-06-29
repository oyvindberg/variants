package tests

import utest._
import variants.internal.Gen
import scala.collection.immutable.Seq

import scala.meta._

object TransformerTest extends TestSuite {

  val tests = this {
    "ADT with one type param" {

      val before = TestUtils.parseTrait(
        """
@Transformer
trait Adt {

  sealed trait A[T]
  @Include("Adt1")
  sealed trait AA[T]  extends A[T]
  sealed trait AAA[T] extends A[T]
  case class B[T](d1: T, d2: T, u: Int) extends AA[T] @Include("Adt1") with AAA[T] @Include("Adt2")

  @Include("Adt1")
  class C[T](val i: Int, val bs: Seq[B[T]]) extends A[T]

  case class D[T](@Include("Adt1") i: Int, @Exclude("Adt2") j: Int) extends A[T]

  case class E[T](@Include("Adt1") ob: Option[B[T]], @Include("Adt1") as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]
  case class F[T]() extends AAA[T]
}""")

      val actual: Stat = Gen(before, Seq("Adt1", "Adt2")).head

      val expected = TestUtils.parseObject(
        """object Adt1 {
  sealed trait A[T]
  sealed trait AA[T] extends A[T]
  sealed trait AAA[T] extends A[T]
  case class B[T](d1: T, d2: T, u: Int) extends AA[T]
  class C[T](val i: Int, val bs: Seq[B[T]]) extends A[T]
  case class D[T](i: Int, j: Int) extends A[T]
  case class E[T](ob: Option[B[T]], as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]
  case class F[T]() extends AAA[T]
  class Adt1Transformer[Scope, T](implicit newscope: variants.NewScope[Scope, A[T]], SeqFunctor: variants.Functor[Seq], OptionFunctor: variants.Functor[Option]) {
    final def visitA(scope: Scope)(_0: A[T]): A[T] = enterA(scope)(_0 match {
      case x: AAA[T] =>
        visitAAA(scope)(x)
      case x: AA[T] =>
        visitAA(scope)(x)
      case x: C[T] =>
        visitC(scope)(x)
      case x: D[T] =>
        visitD(scope)(x)
      case x: E[T] =>
        visitE(scope)(x)
    })
    final def visitAA(scope: Scope)(_0: AA[T]): AA[T] = enterAA(scope)(_0 match {
      case x: B[T] =>
        visitB(scope)(x)
    })
    final def visitAAA(scope: Scope)(_0: AAA[T]): AAA[T] = enterAAA(scope)(_0 match {
      case x: F[T] =>
        visitF(scope)(x)
    })
    final def visitB(scope: Scope)(_0: B[T]): B[T] = {
      val _1: B[T] = enterB(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: B[T] = new B(d1 = _1.d1, d2 = _1.d2, u = _1.u)
      _2
    }
    final def visitC(scope: Scope)(_0: C[T]): C[T] = {
      val _1: C[T] = enterC(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: C[T] = new C(i = _1.i, bs = SeqFunctor.map(_1.bs)(x => visitB(childScope)(x)))
      _2
    }
    final def visitD(scope: Scope)(_0: D[T]): D[T] = {
      val _1: D[T] = enterD(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: D[T] = new D(i = _1.i, j = _1.j)
      _2
    }
    final def visitE(scope: Scope)(_0: E[T]): E[T] = {
      val _1: E[T] = enterE(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: E[T] = new E(ob = OptionFunctor.map(_1.ob)(x => visitB(childScope)(x)), as = SeqFunctor.map(_1.as)(x => visitA(childScope)(x)), d = visitD(childScope)(_1.d), oe = OptionFunctor.map(_1.oe)(x => visitE(childScope)(x)))
      _2
    }
    final def visitF(scope: Scope)(_0: F[T]): F[T] = {
      val _1: F[T] = enterF(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: F[T] = new F()
      _2
    }
    final def >>(that: Adt1Transformer[Scope, T]): Adt1Transformer[Scope, T] = combine(that)
    final def combine(that: Adt1Transformer[Scope, T]): Adt1Transformer[Scope, T] = {
      val self = this
      new Adt1Transformer[Scope, T] {
        override def enterA(scope: Scope)(_0: A[T]): A[T] = that.enterA(scope)(self.enterA(scope)(_0))
        override def enterAA(scope: Scope)(_0: AA[T]): AA[T] = that.enterAA(scope)(self.enterAA(scope)(_0))
        override def enterAAA(scope: Scope)(_0: AAA[T]): AAA[T] = that.enterAAA(scope)(self.enterAAA(scope)(_0))
        override def enterB(scope: Scope)(_0: B[T]): B[T] = that.enterB(scope)(self.enterB(scope)(_0))
        override def enterC(scope: Scope)(_0: C[T]): C[T] = that.enterC(scope)(self.enterC(scope)(_0))
        override def enterD(scope: Scope)(_0: D[T]): D[T] = that.enterD(scope)(self.enterD(scope)(_0))
        override def enterE(scope: Scope)(_0: E[T]): E[T] = that.enterE(scope)(self.enterE(scope)(_0))
        override def enterF(scope: Scope)(_0: F[T]): F[T] = that.enterF(scope)(self.enterF(scope)(_0))
      }
    }
    def enterA(scope: Scope)(_0: A[T]): A[T] = _0
    def enterAA(scope: Scope)(_0: AA[T]): AA[T] = _0
    def enterAAA(scope: Scope)(_0: AAA[T]): AAA[T] = _0
    def enterB(scope: Scope)(_0: B[T]): B[T] = _0
    def enterC(scope: Scope)(_0: C[T]): C[T] = _0
    def enterD(scope: Scope)(_0: D[T]): D[T] = _0
    def enterE(scope: Scope)(_0: E[T]): E[T] = _0
    def enterF(scope: Scope)(_0: F[T]): F[T] = _0
  }
}

""")

      TestUtils.structurallyEqual(actual, expected)
    }

    "another one" {

      val before = TestUtils.parseTrait(
        """
  @Transformer
  trait Base {
    trait Animal[+T]
    @Include("Two") abstract class LivingAnimal[+T] extends Animal[T]
    case class Rhino[T](@Exclude("One") weight: Int, secrets: Seq[T]) extends LivingAnimal[T] @Include("Two") with Animal[T] @Include("One")
    case class Dino[T](height: Int, @Include("Two") enemy: Option[Animal[T]]) extends Animal[T]
    @Include("Two")
    case object Dodo extends Animal[Nothing]
  }
""")

      val actual: Stat = Gen(before, Seq("One", "Two")).drop(1).head

      val expected = TestUtils.parseObject(
        """
object Two {
  trait Animal[+T]
  abstract class LivingAnimal[+T] extends Animal[T]
  case class Rhino[T](weight: Int, secrets: Seq[T]) extends LivingAnimal[T]
  case class Dino[T](height: Int, enemy: Option[Animal[T]]) extends Animal[T]
  case object Dodo extends Animal[Nothing]
  class TwoTransformer[Scope, T](implicit newscope: variants.NewScope[Scope, Animal[T]], OptionFunctor: variants.Functor[Option]) {
    final def visitAnimal(scope: Scope)(_0: Animal[T]): Animal[T] = enterAnimal(scope)(_0 match {
      case x: Dino[T] =>
        visitDino(scope)(x)
      case x: Dodo.type =>
        visitDodo(scope)(x)
      case x: LivingAnimal[T] =>
        visitLivingAnimal(scope)(x)
    })
    final def visitLivingAnimal(scope: Scope)(_0: LivingAnimal[T]): LivingAnimal[T] = enterLivingAnimal(scope)(_0 match {
      case x: Rhino[T] =>
        visitRhino(scope)(x)
    })
    final def visitDino(scope: Scope)(_0: Dino[T]): Dino[T] = {
      val _1: Dino[T] = enterDino(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: Dino[T] = new Dino(height = _1.height, enemy = OptionFunctor.map(_1.enemy)(x => visitAnimal(childScope)(x)))
      _2
    }
    final def visitDodo(scope: Scope)(_0: Dodo.type): Dodo.type = enterDodo(scope)(_0)
    final def visitRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = {
      val _1: Rhino[T] = enterRhino(scope)(_0)
      lazy val childScope: Scope = newscope.derive(scope, _1)
      val _2: Rhino[T] = new Rhino(weight = _1.weight, secrets = _1.secrets)
      _2
    }
    final def >>(that: TwoTransformer[Scope, T]): TwoTransformer[Scope, T] = combine(that)
    final def combine(that: TwoTransformer[Scope, T]): TwoTransformer[Scope, T] = {
      val self = this
      new TwoTransformer[Scope, T] {
        override def enterAnimal(scope: Scope)(_0: Animal[T]): Animal[T] = that.enterAnimal(scope)(self.enterAnimal(scope)(_0))
        override def enterDino(scope: Scope)(_0: Dino[T]): Dino[T] = that.enterDino(scope)(self.enterDino(scope)(_0))
        override def enterDodo(scope: Scope)(_0: Dodo.type): Dodo.type = that.enterDodo(scope)(self.enterDodo(scope)(_0))
        override def enterLivingAnimal(scope: Scope)(_0: LivingAnimal[T]): LivingAnimal[T] = that.enterLivingAnimal(scope)(self.enterLivingAnimal(scope)(_0))
        override def enterRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = that.enterRhino(scope)(self.enterRhino(scope)(_0))
      }
    }
    def enterAnimal(scope: Scope)(_0: Animal[T]): Animal[T] = _0
    def enterDino(scope: Scope)(_0: Dino[T]): Dino[T] = _0
    def enterDodo(scope: Scope)(_0: Dodo.type): Dodo.type = _0
    def enterLivingAnimal(scope: Scope)(_0: LivingAnimal[T]): LivingAnimal[T] = _0
    def enterRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = _0
  }
}
""")
      TestUtils.structurallyEqual(actual, expected)
    }
  }
}
