package variants

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, implicitConversions}

trait Functor[F[_]] {
  def map[T, U](ts: F[T])(f: T => U): F[U]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly

  implicit object SeqFunctor extends Functor[Seq] {
    override def map[T, U](ts: Seq[T])(f: (T) => U): Seq[U] = ts map f
  }

  implicit object ISeqFunctor extends Functor[collection.immutable.Seq] {
    override def map[T, U](ts: collection.immutable.Seq[T])(f: (T) => U): collection.immutable.Seq[U] = ts map f
  }

  implicit object OptionFunctor extends Functor[Option] {
    override def map[T, U](ts: Option[T])(f: (T) => U): Option[U] = ts map f
  }

  implicit def FutureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[T, U](ts: Future[T])(f: (T) => U): Future[U] = ts map f
  }

  implicit class FunctorOps[F[_]: Functor, T](m: F[T]) {
    def map[U](f: T => U): F[U] = Functor[F].map(m)(f)
  }
}
