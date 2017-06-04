package variants

import scala.concurrent.Future

object testing {

//  @Visitor
  @FunctorAnn
  @Variants("Adt1", "Adt2")
  trait Adt {

    sealed trait A[T]
    @Include("Adt1")
    sealed trait AA[T] extends A[T]
    sealed trait AAA[T] extends A[T]
    case class B[T](d1: T, d2: T) extends AA[T] @Include("Adt1") with AAA[T] @Include("Adt2")

    @Include("Adt1")
    case class C[T](i: Int, bs: Seq[B[T]]) extends A[T]
    case class D[T](@Include("Adt1") i: Int, @Exclude("Adt2") j: Int) extends A[T]

    case class E[T](ob: Option[B[Future[Seq[T]]]], @Include("Adt1") as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]
    case class F[T]() extends AAA[T]
  }
}

object Adt1 {
  sealed trait A[T]
  sealed trait AA[T] extends A[T]()
  sealed trait AAA[T] extends A[T]()
  case class B[T](d1: T, d2: T) extends AA[T]()
  case class C[T](i: Int, bs: Seq[B[T]]) extends A[T]()
  case class D[T](i: Int, j: Int) extends A[T]()
  case class E[T](ob: Option[B[T]], as: Seq[A[T]], d: D[T], oe: Option[E[T]]) extends A[T]()
  case class F[T]() extends AAA[T]()
  abstract class Adt1Visitor[Scope, T](implicit newScope: NewScope[Scope, A[T]]) {
    def visitA(scope: Scope)(_0: A[T]): A[T] = _0 match {
      case x: E[T] =>
        visitE(scope)(x)
      case x: D[T] =>
        visitD(scope)(x)
      case x: C[T] =>
        visitC(scope)(x)
      case x: AAA[T] =>
        visitAAA(scope)(x)
      case x: AA[T] =>
        visitAA(scope)(x)
    }
    def visitAA(scope: Scope)(_0: AA[T]): AA[T] = _0 match {
      case x: B[T] =>
        visitB(scope)(x)
    }
    def visitAAA(scope: Scope)(_0: AAA[T]): AAA[T] = _0 match {
      case x: F[T] =>
        visitF(scope)(x)
    }
    final def visitE(scope: Scope)(_0: E[T]): E[T] = {
      val _1: E[T] = enterE(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: E[T] = _1.copy(ob = _1.ob.map(visitB(childScope)), as = _1.as.map(visitA(childScope)), d = _1.d, oe = _1.oe.map(visitE(childScope)))
      _2
    }
    def enterE(scope: Scope)(_0: E[T]): E[T] = _0
    final def visitF(scope: Scope)(_0: F[T]): F[T] = {
      val _1: F[T] = enterF(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: F[T] = _1.copy()
      _2
    }
    def enterF(scope: Scope)(_0: F[T]): F[T] = _0
    final def visitB(scope: Scope)(_0: B[T]): B[T] = {
      val _1: B[T] = enterB(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: B[T] = _1.copy(d1 = _1.d1, d2 = _1.d2)
      _2
    }
    def enterB(scope: Scope)(_0: B[T]): B[T] = _0
    final def visitC(scope: Scope)(_0: C[T]): C[T] = {
      val _1: C[T] = enterC(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: C[T] = _1.copy(i = _1.i, bs = _1.bs.map(visitB(childScope)))
      _2
    }
    def enterC(scope: Scope)(_0: C[T]): C[T] = _0
    final def visitD(scope: Scope)(_0: D[T]): D[T] = {
      val _1: D[T] = enterD(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: D[T] = _1.copy(i = _1.i, j = _1.j)
      _2
    }
    def enterD(scope: Scope)(_0: D[T]): D[T] = _0
  }
}