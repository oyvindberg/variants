package variants

object O extends App {
  import Adt1._
  val e: E[Double] = E(Some(B(2, 4.4)), Seq(D(42, 21), C(1, Seq(B(1, 2)))), D(2, 5), None)
  val ee = e.copy(oe = Some(e))

  case class V[T](implicit N: Numeric[T], newScope: NewScope[List[A[T]], A[T]]) extends Adt1Visitor[List[A[T]], T] {
    override def enterD(scope: List[A[T]])(_0: D[T]): D[T] = {
      val _1: D[T] = _0.copy(_0.i * 2)
      _1
    }

    override def enterB(scope: List[A[T]])(_0: B[T]): B[T] = {
      val _1 = B(N.times(_0.d1, N.fromInt(2)), _0.d2)
      println(s"Rewriting ${_0} => ${_1} in scope ${scope map (_.getClass.getSimpleName) mkString " => "}")
      _1
    }
  }

  println(V[Double]().visitE(Nil)(ee))
}
