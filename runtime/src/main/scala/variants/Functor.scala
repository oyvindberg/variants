package variants

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, implicitConversions}
import scala.collection.immutable.Seq

trait Functor[M[_]] {
  def map[T, U](ts: M[T])(f: T => U): M[U]
}

object Functor {
  def apply[M[_]: Functor]: Functor[M] = implicitly

  implicit object SeqFunctor extends Functor[Seq] {
    override def map[T, U](ts: Seq[T])(f: (T) => U): Seq[U] = ts map f
  }

  implicit object OptionFunctor extends Functor[Option] {
    override def map[T, U](ts: Option[T])(f: (T) => U): Option[U] = ts map f
  }

  implicit def FutureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[T, U](ts: Future[T])(f: (T) => U): Future[U] = ts map f
  }

  implicit class FunctorOps[M[_]: Functor, T](m: M[T]) {
    def map[U](f: T => U): M[U] = Functor[M].map(m)(f)
  }
}
