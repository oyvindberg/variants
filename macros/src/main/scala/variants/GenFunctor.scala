package variants

import scala.meta._

private[variants] object GenFunctor extends (AdtMetadata => Defn) {
  def functorType(x: Name): Term.Name =
    Term.Name(x.value + "Functor")

  val f = Term.Name("f")

  def double(t: Type.Param): Type.Param =
    t.copy(name = Type.Name(t.name.value * 2))

  override def apply(metadata: AdtMetadata): Seq[Defn.Def] = {
    val externalFunctors: Set[String] =
      metadata.leafs
        .flatMap(
          (leaf: Defn) =>
            leaf.collect {
              case applied @ Type.Apply(Type.Name(tpe), tparams) if !metadata.localNames.contains(tpe) =>
                tparams.size match {
                  case 1 => tpe
                  case n => panic(s"We only support type constructors with one param, $tpe has $n", applied.pos)
                }
          }
        )
        .to[Set]

    metadata.mainTrait.tparams match {
      case Seq(beforeP: Type.Param) =>
        val before:    Type.Name  = Type.Name(beforeP.name.value)
        val adtBefore: Type       = applyType(metadata.mainTrait.name, Seq(beforeP))
        val afterP:    Type.Param = double(beforeP)
        val after:     Type.Name  = Type.Name(afterP.name.value)
        val adtAfter:  Type       = applyType(metadata.mainTrait.name, Seq(afterP))

        val cases: Seq[Case] =
          metadata.leafs.map {
            case obj @ Defn.Object(_, _, _) =>
              p"case ${Names.argP}: ${objectType(obj)} => ${Names.arg}"

            case Defn.Class(_, tpe, tparams, Ctor.Primary(_, _, pss), _) =>
              p"case ${Names.argP}: ${applyTypePat(tpe, tparams)} => ${mapParams(metadata.localNames, Names.arg, Names.type2term(tpe), before, after, pss)}"
          }

        val dependencies: Seq[Term.Param] =
          externalFunctors.to[Seq].zipWithIndex.map {
            case (x, idx) =>
              Term.Param(Seq(Mod.Implicit()),
                         Term.Name("ev" + idx),
                         Some(Type.Apply(Names.Functor, Seq(Type.Name(x)))),
                         None)
          }

        val ret = q"""implicit def ${functorType(metadata.adtName)}(..$dependencies): ${Names.Functor}[${metadata.mainTrait.name}] =
          new ${Names.FunctorC}[${metadata.mainTrait.name}] {
            def map[$beforeP, $afterP](${Names.arg}: $adtBefore)($f: $before => $after): $adtAfter = 
              ${Term.Match(Names.arg, cases)} 
            }
        """

        println(ret)
        Seq(ret)

      case Nil =>
        panic(s"${metadata.mainTrait.name.value} must have exactly one type param to generate a Functor",
              metadata.mainTrait.pos)
    }
  }

  def mapParams(localNames: Seq[String],
                owner:      Term.Name,
                ctor:       Term.Name,
                from:       Type,
                to:         Type,
                pss:        Seq[Seq[Term.Param]]): Term.Apply = {

    def go(paramName: Term.Name)(arg: Type.Arg): Option[Term => Term] = {

      def wrap(f: Term => Term): Term => Term =
        (x: Term) => f(x)

      def optimise(term: Term): Term =
        if (term.syntax === Term.Function(Seq(Term.Param(Nil, Names.Y, None, None)), Term.Apply(`f`, Seq(Names.Y))).syntax) f else term

      arg match {
        case tname: Type.Name =>
          if (tname.syntax === from.syntax) Some(wrap(term => q"$f($term)")) else None

        case applied @ Type.Apply(Type.Name(current), Seq(targ)) =>
          go(paramName)(targ).map { (base: Term => Term) =>
            val inner: Term =
              optimise(Term.Function(Seq(Term.Param(Nil, Names.Y, None, None)), base(Names.Y)))

            if (localNames.contains(current))
              wrap(term => q"this.map($term)($inner)")
            else
              wrap(term => q"${Names.FunctorC}[${Type.Name(current)}].map($term)($inner)")
          }

        case other => unexpected(other)
      }
    }

    def handleParam(p: Term.Param): Term.Arg =
      p match {
        case Term.Param(_, paramName: Term.Name, Some(tpe: Type.Arg), _) =>
          go(paramName)(tpe) match {
            case Some(base) => q"$paramName = ${base(Term.Select(owner, paramName))}"
            case None       => q"$paramName = $owner.$paramName"
          }

        case other => unexpected(other)
      }

    pss.tail.foldLeft(q"$ctor(..${pss.head map handleParam})") {
      case (call, args) => Term.Apply(call, args map handleParam)
    }
  }
}
