trait Functor[F[_]] {
  def fmap[A, B](f: => A => B)(a: F[A]): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](f: F[A => B])(a: F[A]): F[B]

  def fmap[A, B](f: => A => B)(a: F[A]): F[B] = ap(pure(f))(a)
}

trait Profunctor[P[_, _]] {
  def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: P[A, B]): P[AA, BB]

  def lmap[A, B, AA](left: AA => A)(p: P[A, B]): P[AA, B] = dimap(left)(identity[B])(p)
  def rmap[A, B, BB](right: B => BB)(p: P[A, B]): P[A, BB] = dimap(identity[A])(right)(p)
}

case class Star[F[_], A, B](run: A => F[B])
case class Costar[F[_], A, B](run: F[A] => B)
case class Bistar[F[_], G[_], A, B](run: F[A] => G[B]) // should be Prostar (?)

trait Cartesian[P[_, _]] extends Profunctor[P] {
  def first[A, B, C](p: P[A, B]): P[(A, C), (B, C)]
  def second[A, B, C](p: P[A, B]): P[(C, A), (C, B)]
}

trait Cocartesian[P[_, _]] extends Profunctor[P] {
  def left[A, B, C](p: => P[A, B]): P[Either[A, C], Either[B, C]]
  def right[A, B, C](p: => P[A, B]): P[Either[C, A], Either[C, B]]
}

trait Monoidal[P[_, _]] extends Profunctor[P] {
  def par[A, B, C, D](pab: P[A, B])(pcd: P[C, D]): P[(A, C), (B, D)]
  def empty: P[Unit, Unit]
}

//

object Functor {
  def fmap[F[_]: Functor, A, B](f: A => B)(a: F[A]): F[B] = implicitly[Functor[F]].fmap(f)(a)
}

object Applicative {
  def pure[F[_]: Applicative, A](a: A): F[A] = implicitly[Applicative[F]].pure(a)
  def ap[F[_]: Applicative, A, B](f: F[A => B])(a: F[A]): F[B] = implicitly[Applicative[F]].ap(f)(a)
}

object Profunctor {
  def dimap[P[_, _]: Profunctor, A, B, AA, BB](left: AA => A)(right: B => BB)(p: P[A, B]): P[AA, BB] = implicitly[Profunctor[P]].dimap(left)(right)(p)
}

//

object implicits {

  implicit object ListFunctor extends Functor[List] {
    def fmap[A, B](f: => A => B)(a: List[A]): List[B] = a.map(f)
  }

  implicit object ListApplicative extends Applicative[List] {
    def pure[A](a: A): List[A] = List(a)
    def ap[A, B](f: List[A => B])(a: List[A]): List[B] = f match {
      case Nil => Nil
      case fh :: ft => ListFunctor.fmap(fh)(a) ::: ap(ft)(a)
    }
  }

  implicit object ArrowProfunctor extends Profunctor[Function1] {
    def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: A => B): AA => BB =
      right compose p compose left
  }

  implicit object ArrowCartesian extends Cartesian[Function1] {
    def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: A => B): AA => BB =
      right compose p compose left
    def first[A, B, C](p: A => B): Tuple2[A, C] => Tuple2[B, C] = (xy: (A, C)) => (p(xy._1), xy._2)
    def second[A, B, C](p: A => B): Tuple2[C, A] => Tuple2[C, B] = (xy: Tuple2[C, A]) => (xy._1, p(xy._2))
  }

  implicit object ArrowCocartesian extends Cocartesian[Function1] {
    def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: A => B): AA => BB =
      right compose p compose left
    def left[A, B, C](p: => A => B): Either[A, C] => Either[B, C] = (xy: Either[A, C]) => xy match {
      case Left(a) => Left(p(a))
      case Right(c) => Right(c)
    }
    def right[A, B, C](p: => A => B): Either[C, A] => Either[C, B] = (xy: Either[C, A]) => xy match {
      case Left(c) => Left(c)
      case Right(a) => Right(p(a))
    }
  }

  implicit object ArrowMonoidal extends Monoidal[Function1] {
    def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: A => B): AA => BB =
      right compose p compose left
    def par[A, B, C, D](pab: A => B)(pcd: C => D) = (ac: (A, C)) => ac match {
      case (a, c) => (pab(a), pcd(c))
    }
    def empty: Unit => Unit = identity _
  }

  implicit def starProfunctor[F[_]: Functor]: Profunctor[Star[F, *, *]] =
    new Profunctor[Star[F, *, *]] {
      def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: Star[F, A, B]): Star[F, AA, BB] =
        Star(aa => Functor.fmap(right)(p.run(left(aa))))
    }

  implicit def costarProfunctor[F[_]: Functor]: Profunctor[Costar[F, *, *]] =
    new Profunctor[Costar[F, *, *]] {
      def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: Costar[F, A, B]): Costar[F, AA, BB] =
        Costar(aa => right(p.run(Functor.fmap(left)(aa))))
    }

  implicit def bistarProfunctor[F[_]: Functor, G[_]: Functor]: Profunctor[Bistar[F, G, *, *]] =
    new Profunctor[Bistar[F, G, *, *]] {
      def dimap[A, B, AA, BB](left: AA => A)(right: B => BB)(p: Bistar[F, G, A, B]): Bistar[F, G, AA, BB] =
        Bistar(aa => Functor.fmap(right)(p.run(Functor.fmap(left)(aa))))
    }

}
