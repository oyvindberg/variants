package variants

import scala.meta._

private[variants] object GenFunctor extends (AdtMetadata => Defn) {
  val Functor = Type.Select(Term.Name(constants.variants), Type.Name(constants.Functor))
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
    val from: Type.Param =
      metadata.mainTrait.tparams match {
        case Seq(one) => noVariance(one)
        case more => panic(s"Only one type parameter is supported in order to generate a ${constants.Functor}. ${metadata.mainTrait.name.value} has ${more.size}", metadata.mainTrait.pos)
      }

    val to:   Type.Param = repeatType(from)

    val locallyDefinedFunctors: Map[String, FunctorDef] =
      metadata.locallyDefined mapValues {
        case x: Defn.Class  => if (x.is[Mod.Abstract]) FunctorDef.LocalBranch(x, metadata.inheritance(x.name.value)) else FunctorDef.LocalClass(x)
        case x: Defn.Object => FunctorDef.LocalObject(x)
        case x: Defn.Trait  => FunctorDef.LocalBranch(x, metadata.inheritance(x.name.value))
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
                x.inheritees.to[Seq] flatMap localFunctor(locallyDefinedFunctors) sortBy (_.tpe.syntax) flatMap `case`
              )
            ))

          case (_, x: FunctorDef.LocalClass) =>
            Some(functorInstance(x, from, to)(deriveNewInstance(arg, x.tpe, x.defn.ctor.paramss)))

          case _ => None
        }
        .to[Seq]

    defn(
      Nil,
      Type.Name(metadata.adtName.value + "Functors"),
      Nil,
      metadata.externalFunctors.values.map(_.asImplicitParam).to[Seq],
      instances
    )
  }

  def functorInstance(x: FunctorDef, from: Type.Param, to: Type.Param)(body: Term): Defn =
    q"""implicit lazy val ${term2pat(x.functorName)}: $Functor[${x.tpe}] =
          new ${type2ctor(Functor)}[${x.tpe}] {
            def map[$from, $to]($arg: ${applyType(x.tpe, Seq(from))})
                               ($f: ${param2type(from)} => ${param2type(to)})
                               : ${applyType(x.tpe, Seq(to))} = $body
          }"""

  def `case`(fd: FunctorDef): Option[Case] =
    fd match {
      case x: FunctorDef.LocalClass =>
        Some(p"case ${term2pat(arg)}: ${applyTypePat(x.tpe, x.defn.tparams)} => ${x.functorName}.map($arg)($f)")
      case x: FunctorDef.LocalBranch =>
        Some(p"case ${term2pat(arg)}: ${applyTypePat(x.tpe, tparams(x.defn))} => ${x.functorName}.map($arg)($f)")
      case x: FunctorDef.LocalObject =>
        Some(p"case ${term2pat(arg)}: ${x.tpe} => $arg")
      case x: FunctorDef.External =>
        None
    }
}
