package variants.internal

import scala.collection.mutable
import scala.meta.inputs.Position
import scala.meta.{Ctor, Lit, Mod, Term, Tree}

sealed trait RequestedVariant {
  def name:             String
  def possibleVariants: Seq[String]
}

object RequestedVariant {
  final case class Variant(name:      String, possibleVariants: Seq[String]) extends RequestedVariant
  final case class CommonSubset(name: String, possibleVariants: Seq[String]) extends RequestedVariant
}

sealed trait ShouldInclude {

  import ShouldInclude._

  def apply[T <: Tree](ctx: RequestedVariant, t: T): Option[T] =
    (this, ctx) match {
      case (Excludes(forVariants), RequestedVariant.Variant(lit, possibleVariants)) =>
        assertLegalVariantsSpecified(t.pos, possibleVariants, forVariants)
        if (forVariants contains lit) None else Some(t)
      case (Includes(forVariants), RequestedVariant.Variant(lit, possibleVariants)) =>
        assertLegalVariantsSpecified(t.pos, possibleVariants, forVariants)
        if (forVariants contains lit) Some(t) else None
      case _ =>
        Some(t)
    }
}

object ShouldInclude {
  final case class Includes(forVariants: Seq[String]) extends ShouldInclude
  final case class Excludes(forVariants: Seq[String]) extends ShouldInclude
  case object Default extends ShouldInclude

  def assertLegalVariantsSpecified(pos: Position, possibleVariants: Seq[String], forVariants: Seq[String]): Unit =
    forVariants.filterNot(possibleVariants.contains) match {
      case Seq() =>
      case illegalVariants =>
        panic(s"Include/Exclude for variant not defined on containing unit: ${illegalVariants.mkString(", ")}", pos)
    }
}

object HasInclusionAnn {

  def unapply[M <: Mod](mods: Seq[M]): Some[(ShouldInclude, Seq[M])] = {
    val otherMods = mutable.ArrayBuffer.empty[M]
    var ret: ShouldInclude = ShouldInclude.Default

    mods foreach {
      case mod @ Mod.Annot(
            Term.Apply(Ctor.Ref.Name(operation @ (constants.Include | constants.Exclude)), forVariantsLit)) =>
        if (ret != ShouldInclude.Default) panic("Can only include/exclude once", mod.pos)
        else {
          val forVariants: Seq[String] =
            forVariantsLit.map { case Lit.String(value) => value }

          ret = operation match {
            case constants.Include => ShouldInclude.Includes(forVariants)
            case constants.Exclude => ShouldInclude.Excludes(forVariants)
          }
        }

      case other => otherMods += other
    }
    Some((ret, otherMods.to[Seq]))
  }
}
