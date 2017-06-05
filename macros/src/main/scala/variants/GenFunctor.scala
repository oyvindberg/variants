package variants

import scala.collection.breakOut
import scala.meta._

private[variants] object GenFunctor extends (AdtMetadata => Defn.Class) {

  override def apply(metadata: AdtMetadata): Defn.Class = {
    val externalFunctors: Seq[External] =
      metadata.classes.flatMap { leaf =>
        externalFunctorsFor(metadata, leaf)
      }.distinct

    val locallyDefinedFunctors: Map[String, FunctorDef] =
      metadata.locallyDefined.map {
        case (str, leaf: Defn.Class)  => str -> LocalClass(leaf)
        case (str, leaf: Defn.Object) => str -> LocalObject(leaf)
        case (str, leaf: Defn.Trait)  => str -> LocalBranch(leaf, metadata.inheritance(leaf.name.value))
        case (_, other) => unexpected(other)
      }(breakOut)

    val allFunctors: Map[String, FunctorDef] =
      locallyDefinedFunctors ++ externalFunctors.map(e => e.name -> e)

    Defn.Class(
      Nil,
      Type.Name(metadata.adtName.value + "Functors"),
      Nil,
      Ctor.Primary(Nil, Ctor.Ref.Name("this"), Seq(externalFunctors.map(_.asImplicitParam))),
      Template(Nil,
               Nil,
               Term.Param(Nil, Name.Anonymous(), None, None),
               Some(locallyDefinedFunctors.flatMap(_._2.toSeq(allFunctors)).to[Seq]))
    )
  }

  sealed trait FunctorDef {

    /** Foo */
    def typeName: Type.Name

    /** Foo */
    final def typeTerm: Term.Name =
      Names.type2term(typeName)

    /** FooFunctor */
    final def functorName: Term.Name =
      Term.Name(typeName.value + "Functor")

    def toSeq(lookup: Map[String, FunctorDef]): Option[Defn]

    def asCase(f: Term.Name): Option[Case]

    final def asImplicitParam: Term.Param =
      Term.Param(Seq(Mod.Implicit()), functorName, Some(Type.Apply(Names.Functor, Seq(typeName))), None)
  }

  final case class LocalBranch(branch: Defn.Trait, inheritees: Set[Defn]) extends FunctorDef {
    override def typeName: Type.Name = branch.name

    def from: Type.Param = branch.tparams.head
    def to:   Type.Param = Names.double(from)

    def asFunctor(lookup: Map[String, FunctorDef]): Defn = {
      val cases: Seq[Case] =
        inheritees
          .to[Seq]
          .flatMap {
            case x: Defn.Class  => lookup.get(x.name.value)
            case x: Defn.Trait  => lookup.get(x.name.value)
            case x: Defn.Object => lookup.get(x.name.value)
          }
          .sortBy(_.typeName.value)
          .flatMap(_.asCase(Names.f))

      q"""implicit lazy val ${Names.term2pat(functorName)}: ${Names.Functor}[$typeName] =
            new ${Names.type2ctor(Names.Functor)}[$typeName] {
              def map[$from, $to](${Names.arg}: ${applyType(typeName, Seq(from))})
                                 (${Names.f}: ${Names.param2type(from)} => ${Names.param2type(to)})
                                 : ${applyType(typeName, Seq(to))} =
              ${Term.Match(Names.arg, cases)}
              }
          """
    }

    override def toSeq(lookup: Map[String, FunctorDef]): Option[Defn] =
      Some(asFunctor(lookup))

    override def asCase(f: Term.Name): Some[Case] =
      Some(p"case ${Names.argP}: ${applyTypePat(typeName, branch.tparams)} => $functorName.map(${Names.arg})($f)")

  }

  final case class LocalClass(leaf: Defn.Class) extends FunctorDef {

    override def typeName: Type.Name = leaf.name

    def from: Type.Param = leaf.tparams.head
    def to:   Type.Param = Names.double(from)

    def asCase(f: Term.Name): Some[Case] =
      Some(p"case ${Names.argP}: ${applyTypePat(typeName, leaf.tparams)} => $functorName.map(${Names.arg})($f)")

    override def toSeq(lookup: Map[String, FunctorDef]): Option[Defn] =
      Some(asFunctor(lookup))

    def asFunctor(lookup: Map[String, FunctorDef]): Defn =
      q"""implicit lazy val ${Names.term2pat(functorName)}: ${Names.Functor}[$typeName] =
            new ${Names.type2ctor(Names.Functor)}[$typeName] {
              def map[$from, $to](${Names.arg}: ${applyType(typeName, Seq(from))})
                                 (${Names.f}: ${Names.param2type(from)} => ${Names.param2type(to)})
                                 : ${applyType(typeName, Seq(to))} =
                ${genNewInstanceFrom(lookup,
                                     Names.arg,
                                     typeName,
                                     Names.param2type(from),
                                     Names.param2type(to),
                                     leaf.ctor.paramss)}
              }
          """

    def genNewInstanceFrom(lookup: Map[String, FunctorDef],
                           owner:  Term.Name,
                           ctor:   Type.Name,
                           from:   Type,
                           to:     Type,
                           pss:    Seq[Seq[Term.Param]]): Term = {

      def wrap(f: Term => Term): Term => Term =
        (x: Term) => f(x)

      def go(paramName: Term.Name)(arg: Type.Arg): Option[Term => Term] =
        arg match {
          case tname: Type.Name =>
            if (tname.syntax === from.syntax) Some(wrap(term => q"${Names.f}($term)")) else None

          case Type.Apply(Type.Name(current), Seq(targ)) =>
            go(paramName)(targ).map { (base: Term => Term) =>
              val inner: Term =
                base(Names.Y) match {
                  case simple if simple.syntax === Term.Apply(Names.f, Seq(Names.Y)).syntax => //optimize away a lambda
                    Names.f
                  case other =>
                    Term.Function(Seq(Term.Param(Nil, Names.Y, None, None)), other)
                }

              wrap(term => q"${lookup(current).functorName}.map($term)($inner)")
            }

          case other => unexpected(other)
        }

      def copyParam(p: Term.Param): Term.Arg =
        p match {
          case Term.Param(_, paramName: Term.Name, Some(tpe: Type.Arg), _) =>
            go(paramName)(tpe) match {
              case Some(base) => q"$paramName = ${base(Term.Select(owner, paramName))}"
              case None       => q"$paramName = $owner.$paramName"
            }

          case other => unexpected(other)
        }

      pss.tail.foldLeft(q"new ${Names.type2ctor(ctor)}(..${pss.head map copyParam})": Term) {
        case (call, args) => Term.Apply(call, args map copyParam)
      }
    }

  }

  final case class LocalObject(leaf: Defn.Object) extends FunctorDef {

    override def typeName: Type.Name = objectType(leaf)

    def asCase(f: Term.Name): Some[Case] =
      Some(p"case ${Names.argP}: $typeName => ${Names.arg}")

    override def toSeq(lookup: Map[String, FunctorDef]): Option[Defn.Def] =
      None
  }

  final case class External(name: String) extends FunctorDef {
    override def toSeq(lookup: Map[String, FunctorDef]): Option[Defn.Def] =
      None

    override def asCase(f: Term.Name): Option[Case] = None

    override def typeName: Type.Name =
      Type.Name(name)

  }

  private def externalFunctorsFor(metadata: AdtMetadata, leaf: Defn): Set[External] =
    leaf
      .collect {
        case applied @ Type.Apply(Type.Name(tpe), tparams) if !metadata.localNames.contains(tpe) =>
          tparams.size match {
            case 1 => External(tpe)
            case n => panic(s"We only support type constructors with one param, $tpe has $n", applied.pos)
          }
      }
      .to[Set]
}
