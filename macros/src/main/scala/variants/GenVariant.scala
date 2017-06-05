package variants

import scala.meta._

private[variants] object GenVariant {
  def apply(currentObject: String, restMods: Seq[Mod], tparams: Seq[Type.Param], stats: Seq[Stat]): Defn = {
    val filter = TreeFilter(currentObject) andThen (_.toSeq)

    defn(Type.Name(currentObject), tparams, Nil, stats flatMap filter)
  }

  case class TreeFilter(thisVersion: String) extends (Stat => Option[Stat]) {
    override def apply(stat: Stat): Option[Stat] =
      stat match {
        case x @ Defn.Trait(InclusionMod(maybe, restMods), _, _, ctor, templ) =>
          maybe(thisVersion,
                x.copy(mods = restMods, ctor = ctor.copy(paramss = paramss(ctor.paramss)), templ = template(templ)))

        case x @ Defn.Class(InclusionMod(maybe, restMods), _, _, ctor: Ctor.Primary, templ) =>
          maybe(thisVersion,
                x.copy(mods = restMods, ctor = ctor.copy(paramss = paramss(ctor.paramss)), templ = template(templ)))

        case x @ Defn.Object(InclusionMod(maybe, restMods), _, templ) =>
          maybe(thisVersion, x.copy(mods = restMods, templ = template(templ)))

        case x @ Defn.Val(InclusionMod(maybe, restMods), _, _, _) =>
          maybe(thisVersion, x.copy(mods = restMods))

        case x @ Defn.Var(InclusionMod(maybe, restMods), _, _, _) =>
          maybe(thisVersion, x.copy(mods = restMods))

        case x @ Defn.Def(InclusionMod(maybe, restMods), _, _, pss, _, _) =>
          maybe(thisVersion, x.copy(mods = restMods, paramss = paramss(pss)))

        case x @ Defn.Type(_, name, _, body: Type) =>
          val newTypes: Seq[Type] =
            With unpack body flatMap {
              case ax @ Type.Annotate(tpe, InclusionMod(maybe, restMods)) =>
                maybe(thisVersion, if (restMods.isEmpty) tpe else ax.copy(annots = restMods))
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
      def maybeParent(p: Term): Option[Ctor.Call] =
        p match {
          case applied @ Term.Apply(fun, _) => maybeParent(fun).map(newFun => applied.copy(fun = newFun))
          case fun @ Term.Annotate(call: Ctor.Call, InclusionMod(maybe, rest)) =>
            maybe(thisVersion, if (rest.isEmpty) call else fun.copy(annots = rest))
          case x: Term.ApplyType => Some(x)
          case other => None
        }

      val newParents: Seq[Ctor.Call] =
        templ.parents flatMap maybeParent

      templ.copy(parents = newParents, stats = templ.stats map (_ flatMap apply))
    }

    def paramss(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Param]] =
      paramss.map(
        _.flatMap {
          case p @ Term.Param(InclusionMod(maybe, restMods), _, _, _) =>
            maybe(thisVersion, p.copy(mods = restMods))
        }
      )
  }
}
