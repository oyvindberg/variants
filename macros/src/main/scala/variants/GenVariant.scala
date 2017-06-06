package variants

import scala.meta._

private[variants] object GenVariant {
  def apply(variant: RequestedVariant, mods: Seq[Mod], tparams: Seq[Type.Param], stats: Seq[Stat]): Defn = {
    val filter = VariantFilter(variant) andThen (_.toSeq)
    defn(mods, Type.Name(variant.name), tparams, Nil, stats flatMap filter)
  }

  case class VariantFilter(variant: RequestedVariant) extends (Stat => Option[Stat]) {
    override def apply(stat: Stat): Option[Stat] =
      stat match {
        case x @ Defn.Trait(HasInclusionAnn(shouldInclude, restMods), _, _, ctor, templ) =>
          shouldInclude(
            variant,
            x.copy(mods = restMods, ctor = ctor.copy(paramss = paramss(ctor.paramss)), templ = template(templ)))

        case x @ Defn.Class(HasInclusionAnn(shouldInclude, restMods), _, _, ctor: Ctor.Primary, templ) =>
          shouldInclude(
            variant,
            x.copy(mods = restMods, ctor = ctor.copy(paramss = paramss(ctor.paramss)), templ = template(templ)))

        case x @ Defn.Object(HasInclusionAnn(shouldInclude, restMods), _, templ) =>
          shouldInclude(variant, x.copy(mods = restMods, templ = template(templ)))

        case x @ Defn.Val(HasInclusionAnn(shouldInclude, restMods), _, _, _) =>
          shouldInclude(variant, x.copy(mods = restMods))

        case x @ Defn.Var(HasInclusionAnn(shouldInclude, restMods), _, _, _) =>
          shouldInclude(variant, x.copy(mods = restMods))

        case x @ Defn.Def(HasInclusionAnn(shouldInclude, restMods), _, _, pss, _, _) =>
          shouldInclude(variant, x.copy(mods = restMods, paramss = paramss(pss)))

        case x @ Defn.Type(_, name, _, body: Type) =>
          val newTypes: Seq[Type] =
            With unpack body flatMap {
              case ax @ Type.Annotate(tpe, HasInclusionAnn(shouldInclude, restMods)) =>
                shouldInclude(variant, if (restMods.isEmpty) tpe else ax.copy(annots = restMods))
              case other => Seq(other)
            }

          /* fall back to `Unit` if there are no types left */
          val newBody: Type =
            (With pack newTypes) getOrElse t"Unit"

          Some(x.copy(body = newBody))

        case other =>
          Some(other)
      }

    def template(templ: Template): Template = {
      def shouldIncludeParent(p: Term): Option[Ctor.Call] =
        p match {
          case applied @ Term.Apply(fun, _) => shouldIncludeParent(fun).map(newFun => applied.copy(fun = newFun))
          case fun @ Term.Annotate(call: Ctor.Call, HasInclusionAnn(shouldInclude, rest)) =>
            shouldInclude(variant, if (rest.isEmpty) call else fun.copy(annots = rest))
          case x: Term.ApplyType => Some(x)
          case x: Ctor.Ref.Name  => Some(x)
          case other =>
            unexpected(other)
            None
        }

      val newParents: Seq[Ctor.Call] =
        templ.parents flatMap shouldIncludeParent

      templ.copy(parents = newParents, stats = templ.stats map (_ flatMap apply))
    }

    def paramss(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Param]] =
      paramss.map(
        _.flatMap {
          case p @ Term.Param(HasInclusionAnn(shouldInclude, restMods), _, _, _) =>
            shouldInclude(variant, p.copy(mods = restMods))
        }
      )
  }
}
