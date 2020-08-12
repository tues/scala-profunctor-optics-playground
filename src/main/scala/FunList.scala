// https://twanvl.nl/blog/haskell/non-regular1

// http://www.cs.ioc.ee/~tarmo/papers/aplas05.pdf
// "General stream functions `Str A → Str B` are thus in natural bijection with maps `Nat ⇒ A → Nat ⇒ B`, which, in turn,
// are in natural bijection with maps `(Nat ⇒ A) × Nat → B`, i.e., `FunArg Nat A → B` where `FunArg S A = (S ⇒ A) × S`."

// https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/cellular-automata
// Relation between FunList, Bazaar and Store comonad

// https://stackoverflow.com/questions/44310458/what-are-simple-definitions-for-control-lens-traversals-partsof-holesof-and-si

sealed trait FunList[A, B, T]
case class Done[A, B, T](t: T) extends FunList[A, B, T]
case class More[A, B, T](a: A, fl: FunList[A, B, B => T]) extends FunList[A, B, T]

object FunList {

  def single[A, B](a: A): More[A, B, B] = More(a, Done(identity))

  def fuse[B, T](bbt: FunList[B, B, T]): T = bbt match {
    case Done(t) => t
    case More(x, l) => fuse(l)(x)
  }

  type EFunList[A, B, T] = Either[T, (A, FunList[A, B, B => T])]

  def out[A, B, T](abt: FunList[A, B, T]): EFunList[A, B, T] = abt match {
    case Done(t) => Left(t)
    case More(x, l) => Right((x, l))
  }

  def inn[A, B, T](eith: EFunList[A, B, T]): FunList[A, B, T] = eith match {
    case Left(t) => Done(t)
    case Right((x, l)) => More(x, l)
  }

  def traverse[A, B, C, T, P[_, _]](p: => P[A, B])(implicit cocart: Cocartesian[P], monoid: Monoidal[P]): P[FunList[A, C, T], FunList[B, C, T]] = {
    val lmap: (FunList[A, C, T] => EFunList[A, C, T]) = out[A, C, T] _
    val rmap: (EFunList[B, C, T] => FunList[B, C, T]) = inn[B, C, T] _
    def rec(pp: => P[A, B]): P[FunList[A, C, C => T], FunList[B, C, C => T]] = traverse(pp)
    def mid(): P[EFunList[A, C, T], EFunList[B, C, T]] = cocart.right(monoid.par(p)(rec(p)))

    cocart.dimap(lmap)(rmap)(mid)
  }

  implicit def funListFunctor[X, Y]: Functor[FunList[X, Y, *]] = new Functor[FunList[X, Y, *]] {
    def fmap[A, B](f: => A => B)(a: FunList[X, Y, A]): FunList[X, Y, B] = a match {
      case Done(t) => Done(f(t))
      case More(x, l) =>
        def compf(g: Y => A) = f.compose(g)
        More(x, fmap(compf _)(l))
    }
  }

  implicit def funListApplicative[X, Y]: Applicative[FunList[X, Y, *]] = new Applicative[FunList[X, Y, *]] {
    def pure[A](a: A): FunList[X, Y, A] = Done(a)

    // Need this to avoid infinite recursion (default `Applicative.fmap` uses `ap`)
    override def fmap[A, B](f: => A => B)(a: FunList[X, Y, A]): FunList[X, Y, B] = a match {
      case Done(t) => Done(f(t))
      case More(x, l) =>
        def compf(g: Y => A) = f.compose(g)
        More(x, fmap(compf _)(l))
    }

    def ap[A, B](f: FunList[X, Y, A => B])(a: FunList[X, Y, A]): FunList[X, Y, B] = f match {
      case Done(g) =>
        fmap(g)(a)
      case More(x, l) =>
        def flip(yab: Y => (A => B)): A => (Y => B) = (a: A) => ((y: Y) => yab(y)(a))
        More(x, ap(fmap(flip)(l))(a))
    }
  }

}
