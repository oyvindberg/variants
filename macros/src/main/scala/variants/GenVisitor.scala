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
              case x: Defn.Object => matchOn(x.name, objectType(x), Nil)
              case x: Defn.Class  => matchOn(Term.Name(x.name.value), x.name, x.tparams)
              case x: Defn.Trait  => matchOn(Term.Name(x.name.value), x.name, x.tparams)
              case other => unexpected(other)
            }

          val `match` = if (cases.nonEmpty) Term.Match(first, cases) else first

          val appliedType = applyType(tname, tparams)
          q"def ${visitMethod(tname)}($scope: $Scope)($first: $appliedType): $appliedType = ${`match`}"
      }

    val leafDefs: Seq[Defn.Def] =
      metadata.leafs.flatMap {
        case o @ Defn.Object(_, name, _) =>
          val tname = objectType(o)

          val visit = q"""
                final def ${visitMethod(name)}($scope: $Scope)($first: $tname): $tname =
                  ${enterMethod(name)}($scope)($first)
                """

          val enter = q"def ${enterMethod(name)}($scope: $Scope)($first: $tname): $tname = $first"

          Seq(visit, enter)

        case Defn.Class(_, tpe, tparams: Seq[Type.Param], Ctor.Primary(_, _, pss), _) =>
          val appliedType = applyType(tpe, tparams)

          val visit = q"""
                final def ${visitMethod(tpe)}($scope: $Scope)($first: $appliedType): $appliedType = {
                  val ${term2pat(second)}: $appliedType = ${enterMethod(tpe)}($scope)($first)
                  lazy val ${term2pat(childScope)}: $Scope = ${instance(NewScope)}.derive($scope, $second)
                  val ${term2pat(third)}: $appliedType =
                    ${genCopy(second, childScope, pss, metadata.locallyDefined.contains)}
                  $third
              }"""

          val enter = q"def ${enterMethod(tpe)}($scope: $Scope)($first: $appliedType): $appliedType = $first"

          Seq(visit, enter)

        case other => unexpected(other)
      }

    val scopeTParam = Type.Param(Nil, Scope, Nil, Type.Bounds(None, None), Nil, Nil)

    q"""abstract class ${visitorType(metadata.adtName)}[$scopeTParam, ..${metadata.mainTrait.tparams}]
              (implicit ${instance(NewScope)}
              : $NewScope[$Scope, ${applyType(metadata.mainTrait.name, metadata.mainTrait.tparams)}]) {
          ..$branchDefs
          ..$leafDefs
        }"""
  }

  def matchOn(termName: Term.Name, typeName: Type.Name, tparams: Seq[Type.Param]): Case =
    Case(
      Pat.Typed({ term2pat(argX) },
                if (tparams.isEmpty) typeName
                else Pat.Type.Apply(typeName, tparams.map(tp => Type.Name(tp.name.value)))),
      None,
      Term.Apply(Term.Apply(visitMethod(termName), Seq(scope)), Seq(argX))
    )

  def genCopy(owner:                Term.Name,
              scope:                Term.Name,
              pss:                  Seq[Seq[Term.Param]],
              isLocallyDefinedName: String => Boolean): Term.Apply = {

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

    pss.tail.foldLeft(q"$owner.copy(..${pss.head map handleParam})") {
      case (call, args) => Term.Apply(call, args map handleParam)
    }
  }
}
