package variants

import scala.meta._

/**
  * An organized way to talk about names and types of the Functors we refer to throughout `GenFunctor` and `GenVisitor`
  */
private [variants] sealed abstract class FunctorDef(/** Foo */ val typeName: Type.Name) {
  /** Foo */
  final def typeTerm: Term.Name =
    type2term(typeName)

  /** FooFunctor */
  final def functorName: Term.Name =
    Term.Name(typeName.value + constants.Functor)

  final def asImplicitParam: Term.Param =
    Term.Param(Seq(Mod.Implicit()), functorName, Some(Type.Apply(GenFunctor.Functor, Seq(typeName))), None)
}

private [variants] object FunctorDef {
  final case class LocalBranch(branch: Defn.Trait, inheritees: Set[Defn]) extends FunctorDef(branch.name)
  final case class LocalClass(leaf: Defn.Class) extends FunctorDef(leaf.name)
  final case class LocalObject(leaf: Defn.Object) extends FunctorDef(objectType(leaf))
  final case class External(name: String) extends FunctorDef(Type.Name(name))
}
