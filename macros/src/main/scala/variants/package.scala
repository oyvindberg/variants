import scala.meta.{Position, Tree, Type}

package object variants {
  private[variants] type Seq[T] = scala.collection.immutable.Seq[T]
  private[variants] val Seq = scala.collection.immutable.Seq

  private[variants] def panic(msg: String, position: Position) =
    throw new RuntimeException(s"Error: $msg at $position")

  private[variants] def unexpected(t: Tree) =
    throw new RuntimeException(s"Error: ${t.syntax} at ${t.pos}: structure: ${t.structure}")

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
}
