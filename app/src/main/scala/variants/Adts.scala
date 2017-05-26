package variants

object testing {
  @Visitors
  @Variants("Adt1", "Adt2")
  trait Adt {
    sealed trait A
    @Include("Adt1")
    sealed trait AA
    type T = A @Include("Adt1") with AA @Include("Adt1")

    case object B extends A

    @Include("Adt1")
    case class C(i: Int, ts: Seq[T]) extends A
    case class D(@Include("Adt1") i: Int, @Exclude("Adt2") j: Int) extends A
  }

}
