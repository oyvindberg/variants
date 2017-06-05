package variants

import scala.meta._

sealed trait FunctorDef {

  /** Foo */
  def typeName: Type.Name

  /** Foo */
  final def typeTerm: Term.Name =
    type2term(typeName)

  /** FooFunctor */
  final def functorName: Term.Name =
    Term.Name(typeName.value + "Functor")

  def toSeq(lookup: Map[String, FunctorDef]): Option[Defn]

  def asCase(f: Term.Name): Option[Case]

  final def asImplicitParam: Term.Param =
    Term.Param(Seq(Mod.Implicit()), functorName, Some(Type.Apply(FunctorDef.Functor, Seq(typeName))), None)
}

object FunctorDef {
  val Functor = Type.Name(constants.Functor)
  val f       = Term.Name("f")
  val arg     = Term.Name("x")

  def repeatType(t: Type.Param): Type.Param =
    t.copy(name = Type.Name(t.name.value * 2))

  final case class LocalBranch(branch: Defn.Trait, inheritees: Set[Defn]) extends FunctorDef {
    override def typeName: Type.Name = branch.name

    def from: Type.Param = branch.tparams.head
    def to:   Type.Param = repeatType(from)

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
          .flatMap(_.asCase(f))

      q"""implicit lazy val ${term2pat(functorName)}: $Functor[$typeName] =
            new ${type2ctor(Functor)}[$typeName] {
              def map[$from, $to]($arg: ${applyType(typeName, Seq(from))})
                                 ($f: ${param2type(from)} => ${param2type(to)})
                                 : ${applyType(typeName, Seq(to))} =
              ${Term.Match(arg, cases)}
              }
          """
    }

    override def toSeq(lookup: Map[String, FunctorDef]): Option[Defn] =
      Some(asFunctor(lookup))

    override def asCase(f: Term.Name): Some[Case] =
      Some(p"case ${term2pat(arg)}: ${applyTypePat(typeName, branch.tparams)} => $functorName.map($arg)($f)")
  }

  final case class LocalClass(leaf: Defn.Class) extends FunctorDef {

    override def typeName: Type.Name = leaf.name

    def from: Type.Param = leaf.tparams.head
    def to:   Type.Param = repeatType(from)

    def asCase(f: Term.Name): Some[Case] =
      Some(p"case ${term2pat(arg)}: ${applyTypePat(typeName, leaf.tparams)} => $functorName.map($arg)($f)")

    override def toSeq(lookup: Map[String, FunctorDef]): Option[Defn] =
      Some(asFunctor(lookup))

    def asFunctor(lookup: Map[String, FunctorDef]): Defn =
      q"""implicit lazy val ${term2pat(functorName)}: $Functor[$typeName] =
            new ${type2ctor(Functor)}[$typeName] {
              def map[$from, $to]($arg: ${applyType(typeName, Seq(from))})
                                 ($f: ${param2type(from)} => ${param2type(to)})
                                 : ${applyType(typeName, Seq(to))} =
                ${genNewInstanceFrom(lookup, arg, typeName, param2type(from), param2type(to), leaf.ctor.paramss)}
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

      def nestedMapAppliesFor(param: Term.Name)(tpe: Type.Arg): Option[Term => Term] =
        tpe match {
          case tname: Type.Name =>
            if (tname.syntax === from.syntax) Some(wrap(term => q"$f($term)")) else None

          case Type.Apply(Type.Name(current), Seq(nextTpe)) =>
            nestedMapAppliesFor(param)(nextTpe).map { (base: Term => Term) =>
              val inner: Term =
                base(arg) match {
                  case simple if simple.syntax === Term.Apply(f, Seq(arg)).syntax => //optimize away a lambda
                    f
                  case other =>
                    Term.Function(Seq(Term.Param(Nil, arg, None, None)), other)
                }

              wrap(term => q"${lookup(current).functorName}.map($term)($inner)")
            }

          case other => unexpected(other)
        }

      def copyParam(p: Term.Param): Term.Arg =
        p match {
          case Term.Param(_, paramName: Term.Name, Some(tpe: Type.Arg), _) =>
            nestedMapAppliesFor(paramName)(tpe) match {
              case Some(base) => q"$paramName = ${base(Term.Select(owner, paramName))}"
              case None       => q"$paramName = $owner.$paramName"
            }

          case other => unexpected(other)
        }

      pss.tail.foldLeft(q"new ${type2ctor(ctor)}(..${pss.head map copyParam})": Term) {
        case (call, args) => Term.Apply(call, args map copyParam)
      }
    }
  }

  final case class LocalObject(leaf: Defn.Object) extends FunctorDef {

    override def typeName: Type.Name = objectType(leaf)

    def asCase(f: Term.Name): Some[Case] =
      Some(p"case ${term2pat(arg)}: $typeName => $arg")

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
}
