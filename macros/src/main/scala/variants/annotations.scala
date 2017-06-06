package variants

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._

@compileTimeOnly(s"`${internal.constants.Include}` can not be used here")
class Include(targets: String*) extends StaticAnnotation

@compileTimeOnly(s"`${internal.constants.Exclude}` can not be used here")
class Exclude(targets: String*) extends StaticAnnotation

@compileTimeOnly(s"`${internal.constants.Transformer}` must be used parallel to `Variants`")
class Transformer extends StaticAnnotation

@compileTimeOnly(s"`${internal.constants.FunctorAnn}` must be used parallel to `Variants`")
class FunctorAnn extends StaticAnnotation

@compileTimeOnly(s"`${internal.constants.Variants}` can not be used here")
class Variants(variants: String*) extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    val variantLiterals = this match {
      case Term.New(Template(Nil, Seq(Term.Apply(Ctor.Ref.Select(Term.Name("variants"), Ctor.Ref.Name(internal.constants.Variants)), vars)), _, None)) => vars
      case q"new ${Ctor.Name(internal.constants.Variants)}(..$vars)" => vars
    }

    defn match {
      case t: Defn.Trait =>
        val variants = internal.Gen(t, variantLiterals collect { case Lit.String(str) => str })

        val ret = q"..$variants"
        println(ret)
        ret
      case other =>
        internal.panic(s"`${internal.constants.Variants}` can only be used on traits", other.pos)
    }
  }
}