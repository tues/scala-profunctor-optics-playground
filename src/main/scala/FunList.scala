// https://twanvl.nl/blog/haskell/non-regular1
sealed trait FunList[A, B, T]
case class Done[A, B, T](t: T) extends FunList[A, B, T]
case class More[A, B, T](a: A, fl: FunList[A, B, B => T]) extends FunList[A, B, T]

object FunList {

  def single[A, B](a: A): FunList[A, B, B] = More(a, Done(identity))

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

}
