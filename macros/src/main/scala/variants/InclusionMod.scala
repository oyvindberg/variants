package variants

import scala.collection.mutable
import scala.meta.{Ctor, Lit, Mod, Term}

private[variants] sealed trait InclusionMod {
  def apply[T](lit: String, t: => T): Option[T] =
    this match {
      case InclusionMod.Excludes(lits) => if (lits contains lit) None else Some(t)
      case InclusionMod.Includes(lits) => if (lits contains lit) Some(t) else None
      case InclusionMod.Default        => Some(t)
    }
}

private[variants] object InclusionMod {
  case class Includes(lits: Seq[String]) extends InclusionMod
  case class Excludes(lits: Seq[String]) extends InclusionMod
  case object Default extends InclusionMod

  def unapply[M <: Mod](mods: Seq[M]): Some[(InclusionMod, Seq[M])] = {
    val otherMods = mutable.ArrayBuffer.empty[M]
    var inclusion = Default: InclusionMod

    mods foreach {
      case mod @ Mod.Annot(Term.Apply(Ctor.Ref.Name(operation @ (constants.Include | constants.Exclude)), names)) =>
        if (inclusion != Default) panic("Can only include/exclude once", mod.pos)
        else {
          val litargs: Seq[String] =
            names.map { case Lit.String(value) => value }

          inclusion = operation match {
            case constants.Include => Includes(litargs)
            case constants.Exclude => Excludes(litargs)
          }
        }

      case other => otherMods += other
    }
    Some((inclusion, otherMods.to[Seq]))
  }
}
