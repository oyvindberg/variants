package variants

import scala.meta._

/**
  * An organized way to talk about names and types of the Functors we refer to throughout `GenFunctor` and `GenVisitor`
  */
private[variants] sealed abstract class FunctorDef(functorNameStr: String) {
  def tpe: Type

  def functorName: Term.Name =
    Term.Name(functorNameStr + constants.Functor)

  final def asImplicitParam: Term.Param =
    Term.Param(Seq(Mod.Implicit()), functorName, Some(Type.Apply(GenFunctor.Functor, Seq(tpe))), None)
}

private[variants] object FunctorDef {
  final case class LocalBranch(branch: Defn.Trait, inheritees: Set[Defn]) extends FunctorDef(branch.name.value) {
    override val tpe: Type.Name = branch.name
  }
  final case class LocalClass(leaf: Defn.Class) extends FunctorDef(leaf.name.value) {
    override val tpe: Type.Name = leaf.name
  }
  final case class LocalObject(leaf: Defn.Object) extends FunctorDef(leaf.name.value) {
    override val tpe: Type.Singleton = objectType(leaf)
  }
  final case class External(name: String) extends FunctorDef(name) {
    override val tpe: Type.Name = Type.Name(name)
  }
}
