package variants

import scala.meta._

private[variants] object IncludeVisitor extends (Defn.Object => Defn.Object) {
  val Scope       = Type.Name("Scope")
  val ScopeTparam = tparam"Scope"
  val scope       = Term.Name("scope")
  val childScope  = Term.Name("childScope")
  val childScopeP = Pat.Var.Term(childScope)
  val newScope    = Term.Name("newScope")
  val NewScope    = Type.Name("NewScope")
  val arg         = Term.Name("x")
  val argP        = Pat.Var.Term(arg)
  val first       = Term.Name("_0")
  val second      = Term.Name("_1")
  val third       = Term.Name("_2")
  val firstP      = Pat.Var.Term(first)
  val secondP     = Pat.Var.Term(second)
  val thirdP      = Pat.Var.Term(third)

  def visitMethod(x: Name): Term.Name =
    Term.Name("visit" + x.value)

  def enterMethod(x: Name): Term.Name =
    Term.Name("enter" + x.value)

  override def apply(defn: Defn.Object): Defn.Object =
    defn match {
      case Defn.Object(mods, base, t @ Template(_, _, _, Some(stats: Seq[Stat]))) =>
        val metadata = AdtMetadata(stats)

        val branchDefs: Seq[Defn.Def] =
          metadata.branches.to[Seq].map {
            case (ctorName, referring) =>
              val cases: Seq[Case] =
                referring.to[Seq] flatMap {
                  case x: Defn.Object if metadata.locallyDefinedNames(x.name.value) =>
                    Seq(p"case $argP: ${x.name}.type => ${visitMethod(x.name)}($scope)($arg)")

                  case x: Defn.Class if metadata.locallyDefinedNames(x.name.value) =>
                    Seq(p"case $argP: ${x.name} => ${visitMethod(x.name)}($scope)($arg)")

                  case x: Defn.Trait if metadata.locallyDefinedNames(x.name.value) =>
                    Seq(p"case $argP: ${x.name} => ${visitMethod(x.name)}($scope)($arg)")

                  case other =>
                    println(s"$ctorName: Ignoring inheritance $other")
                    Nil
                }

              val `match` = Term.Match(first, cases)
              val tname   = Type.Name(ctorName)

              q"def ${visitMethod(tname)}($scope: $Scope)($first: $tname): $tname = ${`match`}"
          }

        val leafDefs: Seq[Defn.Def] =
          metadata.leafs.flatMap {
            case Defn.Object(_, name, _) =>
              val tname = Type.Name(name.value + ".type")

              val visit = q"""
                final def ${visitMethod(name)}($scope: $Scope)($first: $tname): $tname =
                  ${enterMethod(name)}($scope)($first)
                """

              val enter = q"def ${enterMethod(name)}($scope: $Scope)($first: $tname): $tname = $first"
              Seq(visit, enter)

            case Defn.Class(_, tname, _, Ctor.Primary(_, _, pss), _) =>
              val visit = q"""
                final def ${visitMethod(tname)}($scope: $Scope)($first: $tname): $tname = {
                  val $secondP: $tname = ${enterMethod(tname)}($scope)($first)
                  lazy val $childScopeP: $Scope = $newScope.derive($scope, $second)
                  val $thirdP: $tname = ${visitorCopy(second, childScope, pss, metadata.locallyDefinedNames)}
                  $third
              }"""
              val enter = q"def ${enterMethod(tname)}($scope: $Scope)($first: $tname): $tname = $first"

              Seq(visit, enter)

            case other =>
              println(s"Unexpected: ${other.structure}:")
              Nil
          }

        val VisitorName = Type.Name(base.value + "Visitor")

        val visitor: Defn.Class =
          q"""abstract class $VisitorName[$ScopeTparam]
                (implicit $newScope: $NewScope[$Scope, ${metadata.mainTraitName}]) {
                ..$branchDefs
                ..$leafDefs
              }"""

        defn.copy(templ = t.copy(stats = Some(stats :+ visitor)))

      case other =>
        panic(s"`Visitor` can only be used on traits, not on ${other.structure}", other.pos)
    }

  def visitorCopy(owner:              Term.Name,
                  scope:              Term.Name,
                  pss:                Seq[Seq[Term.Param]],
                  locallyDefinedName: String => Boolean): Term.Apply = {

    def visitMember(p: Term.Param): Term.Arg =
      p match {
        case Term.Param(_, paramName: Term.Name, Some(tpe), _) =>
          tpe match {
            case tname: Type.Name =>
              if (locallyDefinedName(tname.value)) {
                q"$paramName = ${visitMethod(tname)}($scope)($owner.$paramName)"
              } else q"$paramName = $owner.$paramName"

            case Type.Apply(_, Seq(tname: Type.Name)) =>
              if (locallyDefinedName(tname.value)) {
                q"$paramName = $owner.$paramName.map(${visitMethod(tname)}($scope))"
              } else q"$paramName = $owner.$paramName"

            case other =>
              unexpected(other)
          }

        case other =>
          unexpected(other)
      }

    pss.tail.foldLeft(q"$owner.copy(..${pss.head map visitMember})") {
      case (call, args) => Term.Apply(call, args map visitMember)
    }
  }
}
