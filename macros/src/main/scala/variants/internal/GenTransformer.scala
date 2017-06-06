package variants.internal

import scala.meta._

object GenTransformer extends (AdtMetadata => Defn) {
  def visitMethod(x: Name): Term.Name =
    Term.Name("visit" + x.value)

  def enterMethod(x: Name): Term.Name =
    Term.Name("enter" + x.value)

  def transformerType(x: Name): Type.Name =
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
          wrap(term => q"${visitMethod(tname)}($childScope)($term)")
        case tname @ Type.Name(value) if metadata.localNames(value) =>
          wrap(term => q"${visitMethod(tname)}($childScope)($term)")
      }
    }

    val branchDefs: Seq[Defn.Def] =
      metadata.branches.map { x =>
        val cases: Seq[Case] =
          metadata.inheritance
            .get(x.name.value)
            .to[Seq]
            .flatten
            .map {
              case x: Defn.Object =>
                p"case ${term2pat(argX)}: ${objectType(x)} => ${visitMethod(x.name)}($scope)($argX)"
              case x: Defn.Class =>
                p"case ${term2pat(argX)}: ${applyTypePat(x.name, x.tparams)} => ${visitMethod(x.name)}($scope)($argX)"
              case x: Defn.Trait =>
                p"case ${term2pat(argX)}: ${applyTypePat(x.name, x.tparams)} => ${visitMethod(x.name)}($scope)($argX)"
            }
            .sortBy(_.pat.syntax)

        val Tpe = applyType(x.name, tparams(x))

        q"def ${visitMethod(x.name)}($scope: $Scope)($first: $Tpe): $Tpe = ${if (cases.nonEmpty) Term.Match(first, cases)
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

    val tparamsNoVariance: Seq[Type.Param] = metadata.mainTrait.tparams map noVariance

    val newScope =
      param"implicit ${instance(NewScope)}: $NewScope[$Scope, ${applyType(metadata.mainTrait.name, tparamsNoVariance)}]"

    val stats = branchDefs ++ leafDefs

    val implicitParams: Seq[Term.Param] = {
      val usedFunctors: Set[String] = referencedFunctorInstances(stats)
      metadata.externalFunctors.values.filter(e => usedFunctors(e.functorName.value)).map(_.asImplicitParam).to[Seq]
    }

    defn(
      Nil,
      transformerType(metadata.adtName),
      Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil) +: tparamsNoVariance,
      Seq(newScope) ++ implicitParams,
      stats
    )
  }
}
