package variants

object testing {

  @Visitors
  @Variants("Adt1", "Adt2")
  trait Adt {
    sealed trait A
    @Include("Adt1")
    sealed trait AA extends A
    sealed trait AAA extends A
    case class B(d1: Double, d2: Double) extends AA @Include("Adt1") with AAA @Include("Adt2")

    @Include("Adt1")
    case class C(i: Int, bs: Seq[B]) extends A
    case class D(@Include("Adt1") i: Int, @Exclude("Adt2") j: Int) extends A

    case class E(ob: Option[B], @Include("Adt1") as: Seq[A], d: D, oe: Option[E]) extends A
  }
}

object Adtx {
  sealed trait A
  sealed trait AA extends A()
  sealed trait AAA extends A()
  case class B(d1: Double, d2: Double) extends AA()
  case class C(i: Int, bs: Seq[B]) extends A()
  case class D(i: Int, j: Int) extends A()
  case class E(ob: Option[B], as: Seq[A], d: D, oe: Option[E]) extends A()
  abstract class Adt1Visitor[Scope](implicit newScope: NewScope[Scope, A]) {
    def visitA(scope: Scope)(_0: A): A = _0 match {
      case x: E =>
        visitE(scope)(x)
      case x: C =>
        visitC(scope)(x)
      case x: D =>
        visitD(scope)(x)
    }
    def visitAA(scope: Scope)(_0: AA): AA = _0 match {
      case x: B =>
        visitB(scope)(x)
    }
    final def visitB(scope: Scope)(_0: B): B = {
      val _1: B = enterB(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: B = _1.copy(d1 = _1.d1, d2 = _1.d2)
      _2
    }
    def enterB(scope: Scope)(_0: B): B = _0
    final def visitC(scope: Scope)(_0: C): C = {
      val _1: C = enterC(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: C = _1.copy(i = _1.i, bs = _1.bs.map(visitB(childScope)))
      _2
    }
    def enterC(scope: Scope)(_0: C): C = _0
    final def visitD(scope: Scope)(_0: D): D = {
      val _1: D = enterD(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: D = _1.copy(i = _1.i, j = _1.j)
      _2
    }
    def enterD(scope: Scope)(_0: D): D = _0
    final def visitE(scope: Scope)(_0: E): E = {
      val _1: E = enterE(scope)(_0)
      lazy val childScope: Scope = newScope.derive(scope, _1)
      val _2: E = _1.copy(ob = _1.ob.map(visitB(childScope)), as = _1.as.map(visitA(childScope)), d = visitD(childScope)(_1.d), oe = _1.oe.map(visitE(childScope)))
      _2
    }
    def enterE(scope: Scope)(_0: E): E = _0
  }
}