package variants

import scala.meta._

private[variants] abstract class DeriveNewInstance(lookup: Map[String, FunctorDef]) {
  private val arg = Term.Name("x")

  val baseCase: PartialFunction[Type, Term => Term]

  protected def wrap(f: Term => Term): Term => Term =
    (x: Term) => f(x)

  def apply(valuesFromInstance: Term.Name, ctor: Type.Name, pss: Seq[Seq[Term.Param]]): Term =
    pss.tail.foldLeft(q"new ${type2ctor(ctor)}(..${pss.head map copyParam(valuesFromInstance)})": Term) {
      case (call, args) => Term.Apply(call, args map copyParam(valuesFromInstance))
    }

  private def copyParam(valuesFromInstance: Term.Name)(param: Term.Param): Term.Arg =
    param match {
      case Term.Param(_, paramName: Term.Name, Some(tpe: Type.Arg), _) =>
        nestedParamValueExpression(paramName)(tpe) match {
          case Some(base) => Term.Arg.Named(paramName, base(Term.Select(valuesFromInstance, paramName)))
          case None       => Term.Arg.Named(paramName, q"$valuesFromInstance.$paramName")
        }

      case other => unexpected(other)
    }

  private def nestedParamValueExpression(param: Term.Name)(tpe: Type.Arg): Option[Term => Term] =
    tpe match {
      case base: Type if baseCase.isDefinedAt(base) => Some(baseCase(base))

      case applied@Type.Apply(Type.Name(current), Seq(nextTpe)) =>
        nestedParamValueExpression(param)(nextTpe).map { (base: Term => Term) =>
          lookup.get(current) match {
            case Some(functor) => wrap(term => q"${functor.functorName}.map($term)((${param"$arg"}) => ${base(arg)})")
            case None => panic(s"Expected to find a Functor instance for $current", applied.pos)
          }
        }

      case _ =>
        None
    }
}
