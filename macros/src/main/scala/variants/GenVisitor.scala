package variants

import variants.FunctorDef.External

import scala.meta._

private[variants] object GenVisitor extends (AdtMetadata => Defn) {
  def visitMethod(x: Name): Term.Name =
    Term.Name("visit" + x.value)

  def enterMethod(x: Name): Term.Name =
    Term.Name("enter" + x.value)

  def visitorType(x: Name): Type.Name =
    Type.Name(x.value + "Visitor")

  val NewScope   = Type.Name(constants.NewScope)
  val Scope      = Type.Name("Scope")
  val scope      = instance(Scope)
  val first      = Term.Name("_0")
  val second     = Term.Name("_1")
  val third      = Term.Name("_2")
  val childScope = Term.Name("childScope")
  val argX       = Term.Name("x")

  override def apply(metadata: AdtMetadata): Defn = {
    val externalFunctors: Map[String, External] =
      metadata.externalTypeCtors
        .mapValues {
          case applied @ Type.Apply(Type.Name(name), tparams) =>
            tparams.size match {
              case 1 => External(name)
              case n => panic(s"We only support type constructors with one param, $name has $n", applied.pos)
            }
        }

    object DeriveNewInstance extends DeriveNewInstance(externalFunctors){
      val baseCase: PartialFunction[Type, Term => Term] = {
        case Type.Apply(tname@Type.Name(value), _) if metadata.localNames(value) =>
          wrap(term => q"${visitMethod(tname)}($childScope)($term)")
        case tname@Type.Name(value) if metadata.localNames(value) =>
          wrap(term => q"${visitMethod(tname)}($childScope)($term)")
      }
    }

    val branchDefs: Seq[Defn.Def] =
      metadata.branches.map {
        case Defn.Trait(_, tname, tparams, _, _) =>
          val cases: Seq[Case] =
            metadata.inheritance.get(tname.value).to[Seq].flatten.map {
              case x: Defn.Object =>
                p"case ${term2pat(argX)}: ${objectType(x)} => ${visitMethod(x.name)}($scope)($argX)"
              case x: Defn.Class =>
                p"case ${term2pat(argX)}: ${applyTypePat(x.name, x.tparams)} => ${visitMethod(x.name)}($scope)($argX)"
              case x: Defn.Trait =>
                p"case ${term2pat(argX)}: ${applyTypePat(x.name, x.tparams)} => ${visitMethod(x.name)}($scope)($argX)"
            }

          val Tpe = applyType(tname, tparams)

          q"def ${visitMethod(tname)}($scope: $Scope)($first: $Tpe): $Tpe = ${if (cases.nonEmpty) Term.Match(first, cases)
          else first}"
      }

    val leafDefs: Seq[Defn.Def] =
      metadata.leafs.flatMap {
        case o @ Defn.Object(_, name, _) =>
          val Tpe = objectType(o)
          Seq(
            q"final def ${visitMethod(name)}($scope: $Scope)($first: $Tpe): $Tpe = ${enterMethod(name)}($scope)($first)",
            q"def ${enterMethod(name)}($scope: $Scope)($first: $Tpe): $Tpe = $first"
          )

        case Defn.Class(_, tpe, tparams: Seq[Type.Param], Ctor.Primary(_, _, pss), _) =>
          val Tpe = applyType(tpe, tparams)
          Seq(
            q"def ${enterMethod(tpe)}($scope: $Scope)($first: $Tpe): $Tpe = $first",
            q"""
                final def ${visitMethod(tpe)}($scope: $Scope)($first: $Tpe): $Tpe = {
                  val ${term2pat(second)}: $Tpe = ${enterMethod(tpe)}($scope)($first)
                  lazy val ${term2pat(childScope)}: $Scope = ${instance(NewScope)}.derive($scope, $second)
                  val ${term2pat(third)}: $Tpe = ${DeriveNewInstance(second, tpe, pss)}
                  $third
              }"""
          )
      }

    val newScope =
      param"implicit ${instance(NewScope)}: $NewScope[$Scope, ${applyType(metadata.mainTrait.name, metadata.mainTrait.tparams)}]"

    defn(
      visitorType(metadata.adtName),
      Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil) +: metadata.mainTrait.tparams,
      Seq(newScope) ++ externalFunctors.values.map(_.asImplicitParam),
      branchDefs ++ leafDefs
    )
  }
}
