package variants

object O extends App {
  import Adtx._

  val e = E(Some(B(2.2, 4.4)), Seq(D(42, 21), C(1, Seq(B(1, 2)))), D(2, 5), None)
  val ee = e.copy(oe = Some(e))
  println(E)

  object V extends Adt1Visitor[List[A]] {
    override def enterD(scope: List[A])(_0: D): D = {
      val _1 = _0.copy(_0.i * 2)
      println(s"Rewriting ${_0} => ${_1} in scope ${scope mkString " => "}")
      _1
    }

  }

  println(V.visitE(Nil)(ee))
}
