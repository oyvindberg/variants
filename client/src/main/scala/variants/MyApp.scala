package variants

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object O extends App {
  case class XX(i: Int)
  object Adt1 extends testing.Adt1[XX]
  import Adt1._

  val e: E[Double] = E(Some(B(2.2, 4.4, XX(22))), Seq(D(42, 21), C(1, Seq(B(1, 2, XX(99))))), D(2, 5), None)
  val ee = e.copy(oe = Some(e))

  case class V[T](implicit N: Numeric[T], newScope: NewScope[List[A[T]], A[T]]) extends Adt1Visitor[List[A[T]], T] {
    override def enterD(scope: List[A[T]])(_0: D[T]): D[T] = {
      val _1: D[T] = _0.copy(_0.i * 2)
      _1
    }

    override def enterB(scope: List[A[T]])(_0: B[T]): B[T] = {
      val _1 = B(N.times(_0.d1, N.fromInt(2)), _0.d2, XX(99))
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
