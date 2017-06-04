package variants

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._

@compileTimeOnly(s"`${constants.Include}` can not be used here")
class Include(targets: String*) extends StaticAnnotation

@compileTimeOnly(s"`${constants.Exclude}` can not be used here")
class Exclude(targets: String*) extends StaticAnnotation

@compileTimeOnly(s"`${constants.Visitor}` must be used parallel to `Variants`")
class Visitor extends StaticAnnotation

@compileTimeOnly(s"`${constants.FunctorAnn}` must be used parallel to `Variants`")
class FunctorAnn extends StaticAnnotation

@compileTimeOnly(s"`${constants.Variants}` can not be used here")
class Variants(variants: String*) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {

    val q"new ${Ctor.Name(constants.Variants)}(..$variantLiterals)" = this

    defn match {
      case Defn.Trait(mods, base, tparams, _, Template(_, _, _, Some(stats))) =>
        val variants: Seq[Stat] =
          variantLiterals.map {
            case Lit.String(variantString) =>
              val variant = GenVariant(variantString, tparams, stats)
              val metadata = AdtMetadata(variant)

              val extras: Seq[Defn] =
                mods flatMap {
                  case x if x.syntax === constants.VisitorAnnot.syntax => Some(GenVisitor(metadata))
                  case x if x.syntax === constants.FunctorAnnot.syntax => Some(GenFunctor(metadata))
                  case _ => None
                }

              variant match {
                case x@Defn.Trait(_, _, _, _, t@Template(_, _, _, Some(stats))) => x.copy(templ = t.copy(stats = Some(stats ++ extras)))
                case x@Defn.Class(_, _, _, _, t@Template(_, _, _, Some(stats))) => x.copy(templ = t.copy(stats = Some(stats ++ extras)))
                case x@Defn.Object(_, _, t@Template(_, _, _, Some(stats))) => x.copy(templ = t.copy(stats = Some(stats ++ extras)))
              }
          }

        q"..$variants"

      case other =>
        panic(s"`${constants.Variants}` can only be used on traits", other.pos)
    }
  }
}