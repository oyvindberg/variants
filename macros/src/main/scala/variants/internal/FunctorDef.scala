package variants.internal

import scala.meta._

/**
  * An organized way to talk about names and types of the Functors we refer to throughout `GenFunctor` and `GenTransformer`
  */
sealed abstract class FunctorDef(functorNameStr: String) {
  def tpe: Type

  def functorName: Term.Name =
    Term.Name(functorNameStr + constants.Functor)

  final def asImplicitParam: Term.Param =
    Term.Param(Seq(Mod.Implicit()), functorName, Some(Type.Apply(GenFunctor.Functor, Seq(tpe))), None)
}

object FunctorDef {
  final case class LocalBranch(defn: Defn with Member.Type, inheritees: Set[Defn])
      extends FunctorDef(defn.name.value) {
    override val tpe: Type.Name = defn.name
  }
  final case class LocalClass(defn: Defn.Class) extends FunctorDef(defn.name.value) {
    override val tpe: Type.Name = defn.name
  }
  final case class LocalObject(defn: Defn.Object) extends FunctorDef(defn.name.value) {
    override val tpe: Type.Singleton = objectType(defn)
  }
  final case class External(name: String) extends FunctorDef(name) {
    override val tpe: Type.Name = Type.Name(name)
  }
}
