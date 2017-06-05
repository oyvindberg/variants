package variants

import scala.meta._

private [variants] object Gen {
  def apply(t: Defn.Trait, variantStrings: Seq[String]): Seq[Stat] =
    variantStrings.map { variantString =>
      val restMods: Seq[Mod] =
        t.mods filter {
          case x if x.syntax === constants.VisitorAnnot.syntax => false
          case x if x.syntax === constants.FunctorAnnot.syntax => false
          case _                                               => true
        }

      val variant  = GenVariant(variantString, restMods, t.tparams, t.templ.stats.getOrElse(Nil))
      val metadata = AdtMetadata(variant)

      val extras: Seq[Defn] =
        t.mods flatMap {
          case x if x.syntax === constants.VisitorAnnot.syntax => Some(GenVisitor(metadata))
          case x if x.syntax === constants.FunctorAnnot.syntax => Some(GenFunctor(metadata))
          case _                                               => None
        }

      variant match {
        case x @ Defn.Trait(_, _, _, _, t @ Template(_, _, _, Some(newStats))) =>
          x.copy(templ = t.copy(stats = Some(newStats ++ extras)))
        case x @ Defn.Class(_, _, _, _, t @ Template(_, _, _, Some(newStats))) =>
          x.copy(templ = t.copy(stats = Some(newStats ++ extras)))
        case x @ Defn.Object(_, _, t @ Template(_, _, _, Some(newStats))) =>
          x.copy(templ = t.copy(stats = Some(newStats ++ extras)))
      }
    }
}