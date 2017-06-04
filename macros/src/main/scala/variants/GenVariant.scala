package variants

import scala.meta._

private[variants] object GenVariant {
  def apply(currentObject: String, tparams: Seq[Type.Param], stats: Seq[Stat]): Defn = {
      val filter     = TreeFilter(currentObject) andThen (_.toSeq)
      val newStats   = stats flatMap filter

      if (tparams.nonEmpty)
        q"""trait ${Type.Name(currentObject)}[..$tparams] { ..$newStats }"""
      else
        q"""object ${Term.Name(currentObject)} { ..$newStats }"""
    }

  case class TreeFilter(thisVersion: String) extends (Stat => Option[Stat]) {
    override def apply(stat: Stat): Option[Stat] =
      stat match {
        case x @ Defn.Trait(InclusionMod(maybe, restMods), _, _, ctor, templ) =>
          maybe(
            thisVersion,
            x.copy(
              mods = restMods,
              ctor = ctor.copy(paramss = paramss(ctor.paramss)),
              templ = template(templ)
            )
          )

        case x @ Defn.Class(InclusionMod(maybe, restMods), _, _, ctor: Ctor.Primary, templ) =>
          maybe(
            thisVersion,
            x.copy(
              mods = restMods,
              ctor = ctor.copy(paramss = paramss(ctor.paramss)),
              templ = template(templ)
            )
          )

        case x @ Defn.Object(InclusionMod(maybe, restMods), _, templ) =>
          maybe(
            thisVersion,
            x.copy(
              mods = restMods,
              templ = template(templ)
            )
          )

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

    def template(templ: Template): Template =
      templ match {
        case Template(_, parents, _, statsOpt) =>
          val newParents: Seq[Ctor.Call] =
            parents flatMap {
              case apply @ Term.Apply(fun @ Term.Annotate(term, InclusionMod(maybe, rest)), _) =>
                maybe(thisVersion, {
                  val newFun = if (rest.isEmpty) term else fun.copy(annots = rest)
                  apply.copy(fun = newFun)
                })
              case other => Seq(other)
            }

          val newStatsOpt: Option[Seq[Stat]] =
            statsOpt.map(_ flatMap apply)

          templ.copy(parents = newParents, stats = newStatsOpt)
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
