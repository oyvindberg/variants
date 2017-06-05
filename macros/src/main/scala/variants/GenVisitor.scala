package variants

import scala.meta._

private[variants] object GenVisitor extends (AdtMetadata => Defn.Class) {
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

  override def apply(metadata: AdtMetadata): Defn.Class = {
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
                  val ${term2pat(third)}: $Tpe = ${genNewInstanceFrom(second,
                                                                      childScope,
                                                                      type2ctor(tpe),
                                                                      pss,
                                                                      metadata.localNames)}
                  $third
              }"""
          )
      }

    val scopeTParam = Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil)

    q"""class ${visitorType(metadata.adtName)}[$scopeTParam, ..${metadata.mainTrait.tparams}]
              (implicit ${instance(NewScope)} : $NewScope[$Scope, ${applyType(metadata.mainTrait.name,
                                                                              metadata.mainTrait.tparams)}]) {
          ..$branchDefs
          ..$leafDefs
        }"""
  }

  def genNewInstanceFrom(owner:                Term.Name,
                         scope:                Term.Name,
                         ctor:                 Ctor.Name,
                         pss:                  Seq[Seq[Term.Param]],
                         isLocallyDefinedName: String => Boolean): Term = {

    def handleParam(p: Term.Param): Term.Arg =
      p match {
        case Term.Param(_, paramName: Term.Name, Some(tpe), _) =>
          tpe match {
            case tname: Type.Name =>
              if (isLocallyDefinedName(tname.value)) {
                q"$paramName = ${visitMethod(tname)}($scope)($owner.$paramName)"
              } else q"$paramName = $owner.$paramName"

            case Type.Apply(_, Seq(arg)) =>
              val tname = arg match {
                case x: Type.Name => x
                case Type.Apply(x: Type.Name, _) => x
              }

              if (isLocallyDefinedName(tname.value)) {
                q"$paramName = $owner.$paramName.map(${visitMethod(tname)}($scope))"
              } else q"$paramName = $owner.$paramName"

            case other => unexpected(other)
          }

        case other => unexpected(other)
      }

    pss.tail.foldLeft(q"new $ctor(..${pss.head map handleParam})": Term) {
      case (call, args) => Term.Apply(call, args map handleParam)
    }
  }
}
