package variants

import scala.meta._

private[variants] object Names {

  def double(t: Type.Param): Type.Param =
    t.copy(name = Type.Name(t.name.value * 2))

  val NewScope = Type.Name(constants.NewScope)
  val Functor  = Type.Name(constants.Functor)

  val childScope = Term.Name("childScope")
  val f          = Term.Name("f")
  val arg        = Term.Name("x")
  val Y          = Term.Name("y")
}
