package variants

object testing {

  @Visitor
  @FunctorAnn
  @Variants("Adt1", "Adt2")
  trait Adt[U] {

    sealed trait A[T]
    @Include("Adt1")
    sealed trait AA[T]  extends A[T]
    sealed trait AAA[T] extends A[T]
    case class B[T](d1: T, d2: T, u: U) extends AA[T] @Include("Adt1") with AAA[T] @Include("Adt2")

    @Include("Adt1")
    case class C[T](i:                  Int, bs:                 Seq[B[T]]) extends A[T]
    case class D[T](@Include("Adt1") i: Int, @Exclude("Adt2") j: Int)       extends A[T]

    case class E[T](@Include("Adt1") ob: Option[B[T]],
                    @Include("Adt1") as: Seq[A[T]],
                    d:                   D[T],
                    oe:                  Option[E[T]])
        extends A[T]
    case class F[T]() extends AAA[T]
  }
}

