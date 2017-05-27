package variants

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._

@compileTimeOnly(s"`${constants.Include}` can not be used here")
class Include(targets: String*) extends StaticAnnotation

@compileTimeOnly(s"`${constants.Exclude}` can not be used here")
class Exclude(targets: String*) extends StaticAnnotation

@compileTimeOnly(s"`${constants.Visitors}` must be used parallel to `Variants`")
class Visitors extends StaticAnnotation

@compileTimeOnly(s"`${constants.Variants}` can not be used here")
class Variants(variants: String*) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val VisitorsAnnot = Mod.Annot(Ctor.Ref.Name(constants.Visitors))

    val q"new ${Ctor.Name(constants.Variants)}(..$variantLiterals)" = this

    defn match {
      case Defn.Trait(mods, base, _, _, Template(_, _, _, Some(stats))) =>
        val variants: Seq[String] =
          variantLiterals.map { case Lit.String(value) => value }

        val out: Seq[Defn.Object] =
          GenVariants(variants, stats)

        val outWithVisitor: Seq[Defn] =
          if (mods exists (_.syntax == VisitorsAnnot.syntax)) {
            out map IncludeVisitor
          } else out

        outWithVisitor foreach println

        q"..$outWithVisitor"

      case other =>
        panic(s"`${constants.Variants}` can only be used on traits", other.pos)
    }
  }
}