package variants

import scala.annotation.tailrec
import scala.meta.{Ctor, Defn, Name, Stat, Template, Term, Type}

private[variants] case class AdtMetadata private (adtName:        Name,
                                                  mainTrait:      Defn.Trait,
                                                  locallyDefined: Map[String, Defn]) {
  lazy val leafs: Seq[Defn] =
    locallyDefined.values.to[Seq].collect {
      case x: Defn.Object => x
      case x: Defn.Class => x
    }

  lazy val branches: Seq[Defn.Trait] =
    locallyDefined.values.to[Seq].collect {
      case x: Defn.Trait => x
    }

  lazy val inheritance: Map[String, Set[Defn]] =
    AdtMetadata.inheritanceMap(locallyDefined)

  lazy val localNames: Seq[String] =
    locallyDefined.keys.to[Seq]
}

private[variants] object AdtMetadata {
  def apply(defn: Defn): AdtMetadata =
    defn match {
      case Defn.Object(_, name, t @ Template(_, _, _, Some(stats)))      => fromStats(name, stats)
      case Defn.Trait(_, name, _, _, t @ Template(_, _, _, Some(stats))) => fromStats(name, stats)
      case Defn.Class(_, name, _, _, t @ Template(_, _, _, Some(stats))) => fromStats(name, stats)
      case other                                                         => unexpected(other)
    }

  def fromStats(adtName: Name, stats: Seq[Stat]): AdtMetadata = {

    val mainTrait: Defn.Trait =
      stats collectFirst { case x: Defn.Trait => x } getOrElse
        panic("Must have a primary trait (defined first) to derive a visitor", stats.head.pos)

    val locallyDefined: Map[String, Defn] =
      stats.collect {
        case x @ Defn.Object(_, Term.Name(name), _)      => name -> x
        case x @ Defn.Class(_, Type.Name(name), _, _, _) => name -> x
        case x @ Defn.Trait(_, Type.Name(name), _, _, _) => name -> x
      }(collection.breakOut)

    def checkTypeParams(name: Name, tparams: Seq[Type.Param]): Unit =
      tparams.foreach { tparam =>
        if (!mainTrait.tparams.exists(that => that.name.syntax === tparam.name.syntax)) {
          panic(
            s"Sub-class ${name.value} can only have params which are defined in base trait:" +
              s"${mainTrait.tparams.map(_.syntax).mkString("[", ", ", "]")}",
            tparam.pos
          )
        }
      }

    locallyDefined foreach {
      case (_, Defn.Trait(_, name, tparams, _, _)) => checkTypeParams(name, tparams)
      case (_, Defn.Class(_, name, tparams, _, _)) => checkTypeParams(name, tparams)
      case _                                       =>
    }

    AdtMetadata(adtName, mainTrait, locallyDefined)
  }

  def inheritanceMap(locallyDefined: Map[String, Defn]): Map[String, Set[Defn]] = {
    @tailrec
    def parentName(c: Term): Ctor.Name =
      c match {
        case name: Ctor.Name => name
        case Ctor.Primary(_, x, _)      => parentName(x)
        case Ctor.Secondary(_, x, _, _) => parentName(x)
        case Term.Apply(x, _)           => parentName(x)
        case Term.ApplyType(x, _)       => parentName(x)
        case other                      => unexpected(other)
      }

    def withParentRefs(acc: Map[String, Set[Defn]], x: Defn, parentRefs: Seq[Ctor.Call]): Map[String, Set[Defn]] =
      parentRefs.foldLeft(acc) {
        case (acc, parentRef) =>
          val parentCtorName: Ctor.Name = parentName(parentRef)

          locallyDefined.get(parentCtorName.value) match {
            case None => acc
            case Some(parent) =>
              val children: Set[Defn] = acc.getOrElse(parentCtorName.value, Set.empty)
              acc + (parentCtorName.value -> (children + x))
          }
      }

    locallyDefined.values.foldLeft[Map[String, Set[Defn]]](Map.empty) {
      case (acc, x @ Defn.Object(_, _, Template(_, parentRefs: Seq[Ctor.Call], _, _))) =>
        withParentRefs(acc, x, parentRefs)
      case (acc, x @ Defn.Class(_, _, _, _, Template(_, parentRefs: Seq[Ctor.Call], _, _))) =>
        withParentRefs(acc, x, parentRefs)
      case (acc, x @ Defn.Trait(_, _, _, _, Template(_, parentRefs: Seq[Ctor.Call], _, _))) =>
        withParentRefs(acc, x, parentRefs)
    }
  }

}
