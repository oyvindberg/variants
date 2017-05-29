package variants

import scala.meta._
import scala.meta.transversers._

private [variants] object GenVariants {
  def apply(variants: Seq[String], stats: Seq[Stat]): Seq[Defn.Object] =
    variants.map { currentObject =>
      val objectName = Term.Name(currentObject)
      val filter = TreeFilter(currentObject)

      q"""
        object $objectName {
          ..${stats flatMap (filter(_))}
        }"""
    }

  case class TreeFilter(thisVersion: String) extends (Stat => Option[Stat]) {
    def apply(stat: Stat): Option[Stat] =
      stat match {
        case x @ Defn.Trait(InclusionMod(include, restMods), _, _, ctor, templ) =>
          include(
            thisVersion,
            x.copy(
              mods = restMods,
              ctor = ctor.copy(paramss = paramss(ctor.paramss)),
              templ = template(templ)
            )
          )

        case x @ Defn.Class(InclusionMod(include, restMods), _, _, ctor: Ctor.Primary, templ) =>
          include(
            thisVersion,
            x.copy(
              mods = restMods,
              ctor = ctor.copy(paramss = paramss(ctor.paramss)),
              templ = template(templ)
            )
          )

        case x @ Defn.Object(InclusionMod(include, restMods), _, templ) =>
          include(
            thisVersion,
            x.copy(
              mods = restMods,
              templ = template(templ)
            )
          )

        case x @ Defn.Val(InclusionMod(include, restMods), _, _, _) =>
          include(thisVersion, x.copy(mods = restMods))

        case x @ Defn.Var(InclusionMod(include, restMods), _, _, _) =>
          include(thisVersion, x.copy(mods = restMods))

        case x @ Defn.Type(_, name, _, body: Type) =>
          val newTypes: Seq[Type] =
            With unpack body flatMap {
              case ax @ Type.Annotate(tpe, InclusionMod(include, restMods)) =>
                include(thisVersion, if (restMods.isEmpty) tpe else ax.copy(annots = restMods))
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
        case Template(_, parents, _, _) =>
          val newParents: Seq[Ctor.Call] =
            parents flatMap {
              case apply @ Term.Apply(fun @ Term.Annotate(term, InclusionMod(include, rest)), _) =>
                include(thisVersion, {
                  val newFun = if (rest.isEmpty) term else fun.copy(annots = rest)
                  apply.copy(fun = newFun)
                })
              case other => Seq(other)
            }
          templ.copy(parents = newParents)
      }

    def paramss(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Param]] =
      paramss.map(
        _.flatMap {
          case p @ Term.Param(InclusionMod(include, restMods), _, _, _) =>
            include(thisVersion, p.copy(mods = restMods))
        }
      )
  }
}
