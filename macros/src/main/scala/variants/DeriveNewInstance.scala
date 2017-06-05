package variants

import scala.meta._

private[variants] abstract class DeriveNewInstance(lookup: Map[String, FunctorDef]) {
  private val arg = Term.Name("x")

  def baseCase: PartialFunction[Type, Term => Term]

  protected def wrap(f: Term => Term): Term => Term =
    (x: Term) => f(x)

  def apply(owner: Term.Name, ctor: Type.Name, pss: Seq[Seq[Term.Param]]): Term =
    pss.tail.foldLeft(q"new ${type2ctor(ctor)}(..${pss.head map copyParam(owner)})": Term) {
      case (call, args) => Term.Apply(call, args map copyParam(owner))
    }

  private def copyParam(owner: Term.Name)(p: Term.Param): Term.Arg =
    p match {
      case Term.Param(_, paramName: Term.Name, Some(tpe: Type.Arg), _) =>
        nestedMapAppliesFor(paramName)(tpe) match {
          case Some(base) => q"$paramName = ${base(Term.Select(owner, paramName))}"
          case None       => q"$paramName = $owner.$paramName"
        }

      case other => unexpected(other)
    }

  private def nestedMapAppliesFor(param: Term.Name)(tpe: Type.Arg): Option[Term => Term] =
    tpe match {
      case base: Type if baseCase.isDefinedAt(base) => Some(baseCase(base))

      case Type.Apply(Type.Name(current), Seq(nextTpe)) =>
        nestedMapAppliesFor(param)(nextTpe).map { (base: Term => Term) =>
          val inner: Term =
            Term.Function(Seq(Term.Param(Nil, arg, None, None)), base(arg))

          wrap(term => q"${lookup(current).functorName}.map($term)($inner)")
        }

      case _ =>
        None
    }
}
