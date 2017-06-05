package variants

import scala.annotation.tailrec
import scala.meta.{Ctor, Defn, Name, Stat, Template, Term, Type}

private[variants] case class AdtMetadata private (adtName:        Name,
                                                  mainTrait:      Defn.Trait,
                                                  locallyDefined: Map[String, Defn]) {
  lazy val localNames: Set[String] =
    locallyDefined.keys.to[Set]

  lazy val localDefs: Seq[Defn] =
    locallyDefined.values.to[Seq]

  lazy val leafs: Seq[Defn] =
    localDefs.collect {
      case x: Defn.Object => x
      case x: Defn.Class  => x
    }

  lazy val classes: Seq[Defn.Class] =
    localDefs.collect {
      case x: Defn.Class => x
    }

  lazy val branches: Seq[Defn.Trait] =
    localDefs.collect {
      case x: Defn.Trait => x
    }

  lazy val inheritance: Map[String, Set[Defn]] =
    AdtMetadata.inheritanceMap(locallyDefined)

  lazy val externalTypeCtors: Map[String, Type.Apply] =
    localDefs.foldLeft(Map.empty[String, Type.Apply]) {
      case (acc, defn) => acc ++ AdtMetadata.externalTypeCtors(defn, localNames)
    }
}

private[variants] object AdtMetadata {
  def apply(defn: Defn): AdtMetadata =
    defn match {
      case Defn.Object(_, name, Template(_, _, _, Some(stats)))      => fromStats(name, stats)
      case Defn.Trait(_, name, _, _, Template(_, _, _, Some(stats))) => fromStats(name, stats)
      case Defn.Class(_, name, _, _, Template(_, _, _, Some(stats))) => fromStats(name, stats)
      case other                                                     => unexpected(other)
    }

  def fromStats(adtName: Name, stats: Seq[Stat]): AdtMetadata = {

    val mainTrait: Defn.Trait =
      stats collectFirst { case x: Defn.Trait => x } getOrElse
        panic(s"ADT in ${adtName.value} must have a primary trait (defined first)", stats.head.pos)

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
            case Some(_) =>
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

  def externalTypeCtors(defn: Defn, isLocallyDefined: String => Boolean): Map[String, Type.Apply] =
    defn.collect {
      case applied @ Type.Apply(Type.Name(tpe), _) if !isLocallyDefined(tpe) => tpe -> applied
    }.toMap

}
