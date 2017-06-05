package variants

import scala.meta._

private[variants] object Names {
  def type2term(x: Type.Name): Term.Name = Term.Name(x.value)
  def type2ctor(x: Type.Name): Ctor.Name = Ctor.Name(x.value)
  def term2type(x: Term.Name): Type.Name = Type.Name(x.value)
  def param2type(x: Type.Param): Type.Name = Type.Name(x.name.value)
  def term2pat(x: Term.Name): Pat.Var.Term = Pat.Var.Term(x)
  def instance(x: Type.Name): Term.Name = Term.Name(x.value.toLowerCase)

  def double(t: Type.Param): Type.Param =
    t.copy(name = Type.Name(t.name.value * 2))

  val Scope       = Type.Name("Scope")
  val scope       = instance(Scope)
  val ScopeTparam = Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil)

  val NewScope    = Type.Name(constants.NewScope)
  val newScope    = instance(NewScope)

  val Functor    = Type.Name(constants.Functor)

  val childScope  = Term.Name("childScope")
  val childScopeP = Pat.Var.Term(childScope)

  val f = Term.Name("f")

  val arg    = Term.Name("x")
  val Y      = Term.Name("y")

  val first  = Term.Name("_0")
  val second = Term.Name("_1")
  val third  = Term.Name("_2")

  val argP    = term2pat(arg)
  val firstP  = term2pat(first)
  val secondP = term2pat(second)
  val thirdP  = term2pat(third)
}
