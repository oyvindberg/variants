package variants

import scala.meta._

private[variants] object GenFunctor extends (AdtMetadata => Defn) {

  override def apply(metadata: AdtMetadata): Defn = {
    val externalFunctors =
      metadata.externalTypeCtors
        .map {
          case (name, applied @ Type.Apply(_, tparams)) =>
            tparams.size match {
              case 1 => FunctorDef.External(name)
              case n => panic(s"We only support type constructors with one param, $name has $n", applied.pos)
            }
        }
        .to[Seq]

    val locallyDefinedFunctors: Map[String, FunctorDef] =
      metadata.locallyDefined map {
        case (str, leaf: Defn.Class)  => str -> FunctorDef.LocalClass(leaf)
        case (str, leaf: Defn.Object) => str -> FunctorDef.LocalObject(leaf)
        case (str, leaf: Defn.Trait)  => str -> FunctorDef.LocalBranch(leaf, metadata.inheritance(leaf.name.value))
        case (_, other) => unexpected(other)
      }

    val allFunctors: Map[String, FunctorDef] =
      locallyDefinedFunctors ++ externalFunctors.map(e => e.name -> e)

    defn(
      Type.Name(metadata.adtName.value + "Functors"),
      Nil,
      externalFunctors.map(_.asImplicitParam),
      locallyDefinedFunctors.flatMap(_._2.toSeq(allFunctors)).to[Seq]
    )
  }
}
