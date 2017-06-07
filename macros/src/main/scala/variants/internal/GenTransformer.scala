package variants.internal

import scala.meta._

object GenTransformer extends (AdtMetadata => Defn) {
  def visitMethodName(x: Name): Term.Name =
    Term.Name("visit" + x.value)

  def enterMethodName(x: Name): Term.Name =
    Term.Name("enter" + x.value)

  def transformerTypeName(x: Name): Type.Name =
    Type.Name(x.value + "Transformer")

  val NewScope   = Type.Select(Term.Name(constants.variants), Type.Name(constants.NewScope))
  val Scope      = Type.Name("Scope")
  val scope      = instance(Scope)
  val first      = Term.Name("_0")
  val second     = Term.Name("_1")
  val third      = Term.Name("_2")
  val childScope = Term.Name("childScope")
  val argX       = Term.Name("x")

  override def apply(metadata: AdtMetadata): Defn = {

    object DeriveNewInstance extends DeriveNewInstance(metadata.externalFunctors) {
      val baseCase: PartialFunction[Type, Term => Term] = {
        case Type.Apply(tname @ Type.Name(value), _) if metadata.localNames(value) =>
          wrap(term => q"${visitMethodName(tname)}($childScope)($term)")
        case tname @ Type.Name(value) if metadata.localNames(value) =>
          wrap(term => q"${visitMethodName(tname)}($childScope)($term)")
      }
    }

    val TransformerType: Type.Name = transformerTypeName(metadata.adtName)
    val tparamsNoVariance: Seq[Type.Param] =
      metadata.mainTrait.tparams map noVariance

    val entryDefs: Seq[Defn.Def] = metadata.localDefs map createEntryDef

    val methods: Seq[Defn.Def] =
      (metadata.branches map createBranchVisitDef(metadata.inheritance)) ++
        (metadata.leafs map createLeafVisitDef(DeriveNewInstance)) ++
        createCombineMethods(entryDefs, TransformerType) ++
        entryDefs


    val implicitParams: Seq[Term.Param] = {
      val usedFunctors: Set[String] =
        referencedFunctorsIn(methods)

      val externalFunctors: Seq[Term.Param] =
        metadata.externalFunctors.values.filter(e => usedFunctors(e.functorName.value)).map(_.asImplicitParam).to[Seq]

      val newScope =
        param"implicit ${instance(NewScope)}: $NewScope[$Scope, ${applyType(metadata.mainTrait.name, tparamsNoVariance)}]"

      Seq(newScope) ++ externalFunctors
    }

    val newTparams: Seq[Type.Param] =
      Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil) +: tparamsNoVariance

    defn(Nil, TransformerType, newTparams, implicitParams, methods)
  }

  private def createCombineMethods(entryDefs: Seq[Defn.Def], TransformerType: Type.Name) = {
    val one = q"final def >>(that: $TransformerType[$Scope]): $TransformerType[$Scope] = combine(that)"
    val two = q"""
      final def combine(that: $TransformerType[$Scope]): $TransformerType[$Scope] = {
        val self = this
        new ${type2ctor(TransformerType)}[$Scope]{
          ..${entryDefs.map(d => d.copy(mods = d.mods :+ Mod.Override(), body = q"that.${d.name}($scope)(self.${d.name}($scope)($first))"))}
        }
      }"""
    Seq(one, two)

  }

  def createLeafVisitDef(newInstanceFrom: DeriveNewInstance)(leaf: Defn): Defn.Def =
    leaf match {
      case o @ Defn.Object(_, name, _) =>
        val Tpe = objectType(o)
        q"final def ${visitMethodName(name)}($scope: $Scope)($first: $Tpe): $Tpe = ${enterMethodName(name)}($scope)($first)"

      case Defn.Class(_, tpe, tparams: Seq[Type.Param], Ctor.Primary(_, _, pss), _) =>
        val Tpe = applyType(tpe, tparams)
        q"""
        final def ${visitMethodName(tpe)}($scope: $Scope)($first: $Tpe): $Tpe = {
          val ${term2pat(second)}: $Tpe = ${enterMethodName(tpe)}($scope)($first)
          lazy val ${term2pat(childScope)}: $Scope = ${instance(NewScope)}.derive($scope, $second)
          val ${term2pat(third)}: $Tpe = ${newInstanceFrom(second, tpe, pss)}
          $third
        }"""
    }

  def createEntryDef(x: Defn): Defn.Def =
    x match {
      case o @ Defn.Object(_, name, _) =>
        val Tpe = objectType(o)
        q"def ${enterMethodName(name)}($scope: $Scope)($first: $Tpe): $Tpe = $first"

      case Defn.Class(_, tpe, tparams: Seq[Type.Param], Ctor.Primary(_, _, pss), _) =>
        val Tpe = applyType(tpe, tparams)
        q"def ${enterMethodName(tpe)}($scope: $Scope)($first: $Tpe): $Tpe = $first"

      case Defn.Trait(_, tpe, tparams: Seq[Type.Param], Ctor.Primary(_, _, pss), _) =>
        val Tpe = applyType(tpe, tparams)
        q"def ${enterMethodName(tpe)}($scope: $Scope)($first: $Tpe): $Tpe = $first"
    }

  def createBranchVisitDef(inheritance: Map[String, Set[Defn]])(x: Defn with Member.Type): Defn.Def = {
    val cases: Seq[Case] =
      inheritance
        .get(x.name.value)
        .to[Seq]
        .flatten
        .map {
          case x: Defn.Object =>
            p"case ${term2pat(argX)}: ${objectType(x)} => ${visitMethodName(x.name)}($scope)($argX)"
          case x: Defn.Class =>
            p"case ${term2pat(argX)}: ${applyTypePat(x.name, x.tparams)} => ${visitMethodName(x.name)}($scope)($argX)"
          case x: Defn.Trait =>
            p"case ${term2pat(argX)}: ${applyTypePat(x.name, x.tparams)} => ${visitMethodName(x.name)}($scope)($argX)"
        }
        .sortBy(_.pat.syntax)

    val Tpe = applyType(x.name, tparams(x))

    val body = Term.Apply(Term.Apply(enterMethodName(x.name), Seq(scope)),
                          Seq(if (cases.nonEmpty) Term.Match(first, cases) else first))
    q"final def ${visitMethodName(x.name)}($scope: $Scope)($first: $Tpe): $Tpe = $body"
  }
}
