package variants

import scala.collection.mutable
import scala.meta.{Ctor, Defn, Stat, Template, Term, Type}

private[variants] case class AdtMetadata(leafs:               Seq[Defn],
                                         branches:            Map[String, Set[Defn]],
                                         locallyDefinedNames: Set[String],
                                         mainTraitName:       Type.Name)

private[variants] object AdtMetadata {
  def apply(stats: Seq[Stat]): AdtMetadata = {
    val leafs               = mutable.ArrayBuffer.empty[Defn]
    val branches            = mutable.HashMap.empty[String, mutable.HashSet[Defn]]
    val locallyDefinedNames = mutable.HashSet.empty[String]
    var mainTraitNameOpt    = Option.empty[String]

    def registerLeaf(name: String, defn: Defn, parents: Seq[Ctor.Call]): Unit = {
      locallyDefinedNames += name
      leafs += defn
      parents foreach {
        case Ctor.Primary(_, Ctor.Name(ctorName), _) =>
          branches.getOrElseUpdate(ctorName, mutable.HashSet.empty) += defn
        case Ctor.Secondary(_, Ctor.Name(ctorName), _, _) =>
          branches.getOrElseUpdate(ctorName, mutable.HashSet.empty) += defn
        case Term.Apply(Ctor.Name(ctorName), _) =>
          branches.getOrElseUpdate(ctorName, mutable.HashSet.empty) += defn
        case other =>
          unexpected(other)
      }
    }

    stats foreach {
      case x @ Defn.Class(_, Type.Name(name), _, _, Template(_, parents: Seq[Ctor.Call], _, _)) =>
        registerLeaf(name, x, parents)
      case x @ Defn.Object(_, Term.Name(name), Template(_, parents, _, _)) =>
        registerLeaf(name, x, parents)
      case x @ Defn.Trait(_, Type.Name(name), _, _, _) =>
        locallyDefinedNames += name
        if (mainTraitNameOpt.isEmpty) {
          mainTraitNameOpt = Some(name)
        }
      case other =>
        Nil
    }

    val mainTraitName: Type.Name =
      mainTraitNameOpt match {
        case Some(name) => Type.Name(name)
        case None       => panic("Must have a primary trait (defined first) to derive a visitor", stats.head.pos)
      }

    AdtMetadata(
      leafs.to[Seq],
      branches.toMap.mapValues(_.to[Set]),
      locallyDefinedNames.to[Set],
      mainTraitName
    )
  }
}
