import scala.meta.{Transformer => _, _}

package object variants {
  private[variants] type Seq[T] = scala.collection.immutable.Seq[T]
  private[variants] val Seq = scala.collection.immutable.Seq

  private[variants] def panic(msg: String, position: Position): Nothing =
    throw new RuntimeException(s"Error: $msg at $position")

  private[variants] def unexpected(t: Tree): Nothing =
    throw new RuntimeException(s"Unexpected: ${t.syntax} at ${t.pos}: structure: ${t.structure}")

  private[variants] def debug(str: String, t: Tree) =
    println(s"$str: ${t.syntax}: ${t.structure}")

  private[variants] implicit class AnyOps[T](private val t: T) {
    def ===(other: T): Boolean = t == other
    def =/=(other: T): Boolean = t != other
  }

  /* handling nested types which are connected with `with` is a bit unwieldy, so... */
  private[variants] object With {
    def unpack(tpe: Type): Seq[Type] =
      tpe match {
        case Type.With(lhs, rhs) => unpack(lhs) ++ unpack(rhs)
        case other               => Seq(other)
      }

    def pack(ts: Seq[Type]): Option[Type] =
      ts.reduceOption((one, two) => Type.With(one, two))
  }

  private[variants] object constants {
    val variants: String = "variants"

    val FunctorAnn:  String = classOf[FunctorAnn].getSimpleName
    val Include:     String = classOf[Include].getSimpleName
    val Exclude:     String = classOf[Exclude].getSimpleName
    val Transformer: String = classOf[Transformer].getSimpleName
    val Variants:    String = classOf[Variants].getSimpleName
    val Functor:     String = classOf[Functor[Functor]].getSimpleName
    val NewScope:    String = classOf[NewScope[_, _]].getSimpleName
  }

  private[variants] def objectType(obj: Defn.Object): Type.Singleton =
    Type.Singleton(obj.name)

  private[variants] def applyType(tpe: Type, tparams: Seq[Type.Param]): Type =
    if (tparams.nonEmpty) Type.Apply(tpe, tparams.map(tp => Type.Name(tp.name.value))) else tpe

  private[variants] def applyTypePat(tpe: Type.Name, tparams: Seq[Type.Param]): Pat.Type =
    if (tparams.nonEmpty) Pat.Type.Apply(tpe, tparams.map(tp => Type.Name(tp.name.value))) else tpe

  private[variants] def type2term(x:  Type.Name):   Term.Name     = Term.Name(x.value)
  private[variants] def type2ctor(x:  Type.Name):   Ctor.Ref.Name = Ctor.Name(x.value)
  private[variants] def type2ctor(x:  Type.Select): Ctor.Ref      = Ctor.Ref.Select(x.qual, type2ctor(x.name))
  private[variants] def term2type(x:  Term.Name):   Type.Name     = Type.Name(x.value)
  private[variants] def param2type(x: Type.Param):  Type.Name     = Type.Name(x.name.value)
  private[variants] def term2pat(x:   Term.Name):   Pat.Var.Term  = Pat.Var.Term(x)
  private[variants] def instance(x:   Type.Name):   Term.Name     = Term.Name(x.value.toLowerCase)
  private[variants] def instance(x:   Type.Select): Term.Name     = instance(x.name)

  private[variants] def defn(mods:    Seq[Mod],
                             name:    Type.Name,
                             tparams: Seq[Type.Param],
                             params:  Seq[Term.Param],
                             stats:   Seq[Stat]): Defn =
    (tparams.isEmpty, params.isEmpty) match {
      case (true, true)  => q"..$mods object ${type2term(name)}{..$stats}"
      case (false, true) => q"..$mods trait $name[..$tparams]{..$stats}"
      case _             => q"..$mods class $name[..$tparams](..$params){..$stats}"
    }

  def noVariance(tp: Type.Param): Type.Param =
    tp.copy(mods = Nil)

  def tparams(defn: Defn with Member.Type): Seq[Type.Param] =
    defn match {
      case x: Defn.Class => x.tparams
      case x: Defn.Trait => x.tparams
    }

  /* ideally we would have returned these correctly in the first place, but this is good enough for now */
  def referencedFunctorInstances(instances: Seq[Defn]): Set[String] = {
    instances.foldLeft(Set.empty[String]) {
      case (acc, defn) => acc ++ defn.collect { case Term.Select(Term.Name(name), Term.Name("map")) => name }
    }
  }
}
