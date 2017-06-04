package variants

import scala.meta._

private[variants] object Names {
  def type2term(x: Type.Name): Term.Name = Term.Name(x.value)
  def type2ctor(x: Type.Name): Ctor.Name = Ctor.Name(x.value)
  def term2type(x: Term.Name): Type.Name = Type.Name(x.value)
  def instance(x: Type.Name): Term.Name = Term.fresh(x.value.toLowerCase)

  val Scope       = Type.Name("Scope")
  val scope       = instance(Scope)
  val ScopeTparam = Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil)

  val NewScope    = Type.Name(constants.NewScope)
  val newScope    = instance(NewScope)

  val Functor    = Type.Name(constants.Functor)
  val FunctorC   = type2ctor(Functor)
  val functor    = instance(Functor)

  val childScope  = Term.Name("childScope")
  val childScopeP = Pat.Var.Term(childScope)

  val arg    = Term.Name("x")
  val Y      = Term.Name("y")

  val first  = Term.Name("_0")
  val second = Term.Name("_1")
  val third  = Term.Name("_2")

  val argP    = Pat.Var.Term(arg)
  val firstP  = Pat.Var.Term(first)
  val secondP = Pat.Var.Term(second)
  val thirdP  = Pat.Var.Term(third)
}
