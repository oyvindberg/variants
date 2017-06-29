package variants.internal

import scala.meta._

object DeriveNewInstance {
  val arg = Term.Name("x")

  def apply(lookupFunctor: Map[String, FunctorDef],
            baseCase:      PartialFunction[Type, Term => Term],
            origin:        Term.Name,
            ctor:          Type.Name,
            pss:           Seq[Seq[Term.Param]]): Term = {

    def copyParam(param: Term.Param): Term.Arg =
      param match {
        case Term.Param(_, paramName: Term.Name, Some(tpe: Type.Arg), _) =>
          nestFunctors(lookupFunctor, baseCase, tpe) match {
            case Some(base) => Term.Arg.Named(paramName, base(Term.Select(origin, paramName)))
            case None       => Term.Arg.Named(paramName, q"$origin.$paramName")
          }

        case other => unexpected(other)
      }

    if (pss.flatten.isEmpty) q"new ${type2ctor(ctor)}()"
    else
      pss.tail.foldLeft(q"new ${type2ctor(ctor)}(..${pss.head map copyParam})": Term) {
        case (call, args) => Term.Apply(call, args map copyParam)
      }
  }

  def nestFunctors(lookupFunctor: Map[String, FunctorDef],
                   baseCase:      PartialFunction[Type, Term => Term],
                   tpe:           Type.Arg): Option[Term => Term] =
    tpe match {
      case base: Type if baseCase.isDefinedAt(base) => Some(baseCase(base))

      case applied @ Type.Apply(Type.Name(currentFunctorName), Seq(nextTpe)) =>
        nestFunctors(lookupFunctor, baseCase, nextTpe).map { (base: Term => Term) =>
          lookupFunctor.get(currentFunctorName) match {
            case Some(functor) => withTerm(term => q"${functor.functorName}.map($term)((${param"$arg"}) => ${base(arg)})")
            case None          => panic(s"Expected to find a Functor instance for $currentFunctorName", applied.pos)
          }
        }

      case _ =>
        None
    }
}
