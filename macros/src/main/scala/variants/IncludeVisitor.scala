package variants

import scala.meta._

private[variants] object IncludeVisitor extends (Defn.Object => Defn.Object) {
  val Scope      = Type.Name("Scope")
  val scope      = Term.Name("scope")
  val childScope = Term.Name("childScope")
  val _0         = Term.Name("_0")
  val _1         = Term.Name("_1")

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
                    val visitName = Term.Name("visit" + x.name.value)
                    Seq(p"case x: ${x.name}.type => $visitName(t)(x)")

                  case x: Defn.Class if metadata.locallyDefinedNames(x.name.value) =>
                    val visitName = Term.Name("visit" + x.name.value)
                    Seq(p"case x: ${x.name} => $visitName(t)(x)")

                  case x: Defn.Trait if metadata.locallyDefinedNames(x.name.value) =>
                    val visitName = Term.Name("visit" + x.name.value)
                    Seq(p"case x: ${x.name} => $visitName(t)(x)")

                  case other =>
                    println(s"$ctorName: Ignoring inheritance $other")
                    Nil
                }

              val `match`   = Term.Match(Term.Name("_0"), cases)
              val name      = Type.Name(ctorName)
              val visitName = Term.Name(s"visit${name.value}")

              q"def $visitName(t: $Scope)(_0: $name): $name = ${`match`}"
          }

        val leafDefs: Seq[Defn.Def] =
          metadata.leafs.flatMap {
            case Defn.Object(_, name, _) =>
              val visitName = Term.Name(s"visit${name.value}")
              val enterName = Term.Name(s"enter${name.value}")

              val visit = q"final def $visitName(scope: $Scope)(_0: $name.type): $name.type = $enterName(scope)(_0)"
              val enter = q"def $enterName(scope: $Scope)(_0: $name.type): $name.type = _0"
              Seq(visit, enter)

            case Defn.Class(_, name, _, Ctor.Primary(_, _, pss), _) =>
              val termName  = Term.Name(name.value)
              val enterName = Term.Name(s"enter${name.value}")
              val visitName = Term.Name(s"visit${name.value}")

              val visit = q"""
                final def $visitName($scope: $Scope)(_0: $name): $name = {
                  val _1: $name = $enterName($scope)(_0)
                  lazy val childScope: $Scope = newScope.derive($scope, _1)
                  val _2: $name = ${visitorCopy(_1, childScope, pss, metadata.locallyDefinedNames)}
                  _2
              }"""
              val enter = q"def $enterName($scope: $Scope)(_0: $name): $name = _0"

              Seq(visit, enter)

            case other =>
              println(s"Unexpected: ${other.structure}:")
              Nil
          }

        val VisitorName = Type.Name(base.value + "Visitor")

        val visitor: Defn.Class =
          q"""abstract class $VisitorName[Scope](implicit newScope: NewScope[$Scope, ${metadata.mainTraitName}]) {
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
            case Type.Name(typeName) =>
              if (locallyDefinedName(typeName)) {
                val visitName = Term.Name("visit" + typeName)
                q"$paramName = $visitName($scope)($owner.$paramName)"
              } else q"$paramName = $owner.$paramName"

            case Type.Apply(_, Seq(Type.Name(typeName))) =>
              if (locallyDefinedName(typeName)) {
                val visitName = Term.Name("visit" + typeName)
                q"$paramName = $owner.$paramName.map($visitName($scope))"
              } else q"$paramName = $owner.$paramName"

            case other =>
              panic(s"${other.structure} not supported", other.pos)
          }

        case other =>
          panic(s"${other.structure} not supported", other.pos)
      }

    pss.tail.foldLeft(q"$owner.copy(..${pss.head map visitMember})") {
      case (call, args) => Term.Apply(call, args map visitMember)
    }
  }
}
