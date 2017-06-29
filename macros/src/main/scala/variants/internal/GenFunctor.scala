package variants.internal

import scala.meta._

object GenFunctor extends (AdtMetadata => Defn) {
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
        case more =>
          panic(
            s"Exactly one type parameter is needed in order to generate a ${constants.Functor}. ${metadata.mainTrait.name.value} has ${more.size}",
            metadata.mainTrait.pos
          )
      }

    val to: Type.Param = repeatType(from)

    val locallyDefinedFunctors: Map[String, FunctorDef] =
      metadata.locallyDefined mapValues {
        case x: Defn.Class =>
          if (x.is[Mod.Abstract]) FunctorDef.LocalBranch(x, metadata.inheritance(x.name.value))
          else FunctorDef.LocalClass(x)
        case x: Defn.Object => FunctorDef.LocalObject(x)
        case x: Defn.Trait  => FunctorDef.LocalBranch(x, metadata.inheritance(x.name.value))
        case other => unexpected(other)
      }

    val fromTparam: Type.Name = param2type(from)

    val instances: Seq[Defn] =
      locallyDefinedFunctors
        .flatMap {
          case (_, x: FunctorDef.LocalBranch) =>
            Some(
              functorInstance(x, from, to)(
                Term.Match(
                  arg,
                  x.inheritees
                    .to[Seq] flatMap localFunctor(locallyDefinedFunctors) sortBy (_.tpe.syntax) flatMap `case`
                )
              ))

          case (_, x: FunctorDef.LocalClass) =>
            val term: Term = DeriveNewInstance(
              locallyDefinedFunctors ++ metadata.externalFunctors,
              {case name: Type.Name if fromTparam.syntax === name.syntax => withTerm(term => q"$f($term)")},
              arg,
              x.tpe,
              x.defn.ctor.paramss
            )
            Some(functorInstance(x, from, to)(term))

          case _ => None
        }
        .to[Seq]

    val implicitParams = {
      val usedFunctors: Set[String] = referencedFunctorsIn(instances)
      metadata.externalFunctors.values.filter(e => usedFunctors(e.functorName.value)).map(_.asImplicitParam).to[Seq]
    }

    defn(
      Nil,
      Type.Name(metadata.adtName.value + "Functors"),
      Nil,
      implicitParams,
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
