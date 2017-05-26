package variants

import scala.meta._

object GenVariants {
  def apply(variants: Seq[String], stats: Seq[Stat]): Seq[Defn.Object] =
    variants.map { currentObject =>
      val objectName = Term.Name(currentObject)
      q"""
        object $objectName {
          ..${stats flatMap filterStatFor(currentObject)}
        }"""
    }

  def filterStatFor(thisVersion: String)(stat: Stat): Option[Stat] =
    stat match {
      case x @ Defn.Trait(InclusionMod(include, restMods), _, _, ctor, _) =>
        include(thisVersion,
                x.copy(mods = restMods, ctor = ctor.copy(paramss = filterParamss(thisVersion)(ctor.paramss))))

      case x @ Defn.Class(InclusionMod(include, restMods), _, _, ctor: Ctor.Primary, _) =>
        include(thisVersion,
                x.copy(
                  mods = restMods,
                  ctor = ctor.copy(paramss = filterParamss(thisVersion)(ctor.paramss))
                ))

      case x @ Defn.Object(InclusionMod(include, restMods), _, _) =>
        include(thisVersion, x.copy(mods = restMods))

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
        println(other.structure)
        Some(other)
    }

  def filterParamss(thisVersion: String)(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Param]] =
    paramss.map(
      _.flatMap {
        case p @ Term.Param(InclusionMod(include, restMods), _, _, _) =>
          include(thisVersion, p.copy(mods = restMods))
      }
    )
}
