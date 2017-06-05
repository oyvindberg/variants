package variants

import scala.meta._

private[variants] object GenFunctor extends (AdtMetadata => Defn) {
  val Functor = Type.Name(constants.Functor)
  val f       = Term.Name("f")
  val arg     = Term.Name("x")

  def repeatType(t: Type.Param): Type.Param =
    t.copy(name = Type.Name(t.name.value * 2))

  def localFunctor(fs: Map[String, FunctorDef])(defn: Defn): Option[FunctorDef] =
    defn match {
      case x: Defn.Class  => fs.get(x.name.value)
      case x: Defn.Trait  => fs.get(x.name.value)
      case x: Defn.Object => fs.get(x.name.value)
      case _ => None
    }

  override def apply(metadata: AdtMetadata): Defn = {

    val Seq(from: Type.Param) = metadata.mainTrait.tparams
    val to:   Type.Param = repeatType(from)

    val locallyDefinedFunctors: Map[String, FunctorDef] =
      metadata.locallyDefined mapValues {
        case leaf: Defn.Class  => FunctorDef.LocalClass(leaf)
        case leaf: Defn.Object => FunctorDef.LocalObject(leaf)
        case leaf: Defn.Trait  => FunctorDef.LocalBranch(leaf, metadata.inheritance(leaf.name.value))
        case other => unexpected(other)
      }

    object deriveNewInstance extends DeriveNewInstance(locallyDefinedFunctors ++ metadata.externalFunctors) {
      val fromTparam: Type.Name = param2type(from)

      override val baseCase: PartialFunction[Type, (Term) => Term] = {
        case name: Type.Name if fromTparam.syntax === name.syntax => wrap(term => q"$f($term)")
      }
    }

    val instances: Seq[Defn] =
      locallyDefinedFunctors
        .flatMap {
          case (_, x: FunctorDef.LocalBranch) =>
            Some(functorInstance(x, from, to)(
              Term.Match(
                arg,
                x.inheritees.to[Seq] flatMap localFunctor(locallyDefinedFunctors) sortBy (_.typeName.value) flatMap `case`
              )
            ))

          case (_, x: FunctorDef.LocalClass) =>
            Some(functorInstance(x, from, to)(deriveNewInstance(arg, x.typeName, x.leaf.ctor.paramss)))

          case _ => None
        }
        .to[Seq]

    defn(
      Type.Name(metadata.adtName.value + "Functors"),
      Nil,
      metadata.externalFunctors.values.map(_.asImplicitParam).to[Seq],
      instances
    )
  }

  def functorInstance(x: FunctorDef, from: Type.Param, to: Type.Param)(body: Term): Defn =
    q"""implicit lazy val ${term2pat(x.functorName)}: $Functor[${x.typeName}] =
          new ${type2ctor(Functor)}[${x.typeName}] {
            def map[$from, $to]($arg: ${applyType(x.typeName, Seq(from))})
                               ($f: ${param2type(from)} => ${param2type(to)})
                               : ${applyType(x.typeName, Seq(to))} = $body
          }"""

  def `case`(fd: FunctorDef): Option[Case] =
    fd match {
      case x: FunctorDef.LocalClass =>
        Some(p"case ${term2pat(arg)}: ${applyTypePat(x.typeName, x.leaf.tparams)} => ${x.functorName}.map($arg)($f)")
      case x: FunctorDef.LocalBranch =>
        Some(p"case ${term2pat(arg)}: ${applyTypePat(x.typeName, x.branch.tparams)} => ${x.functorName}.map($arg)($f)")
      case x: FunctorDef.LocalObject =>
        Some(p"case ${term2pat(arg)}: ${x.typeName} => $arg")
      case x: FunctorDef.External =>
        None
    }
}
