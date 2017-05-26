package variants

trait NewScope[Scope, -Adt]{
  def derive[A <: Adt](scope: Scope, a: A): Scope
}

object NewScope {
  implicit def UnitNewScope[Adt]: NewScope[Unit, Adt] =
    new NewScope[Unit, Adt] {
      override def derive[A <: Adt](scope: Unit, a: A): Unit = scope
    }

  implicit def ListNewScope[Adt]: NewScope[List[Adt], Adt] =
    new NewScope[List[Adt], Adt] {
      override def derive[A <: Adt](scope: List[Adt], a: A): List[Adt] = a :: scope
    }
}