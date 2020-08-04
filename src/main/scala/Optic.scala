trait Lens[A, B, AA, BB] {
  def view(aa: AA): A
  def update(baa: (B, AA)): BB
}

trait LensP[A, B, AA, BB] {
  def apply[P[_, _]](h: P[A, B])(implicit prof: Cartesian[P]): P[AA, BB]
}

object Lens {
  def c2p[A, B, AA, BB](lens: Lens[A, B, AA, BB]): LensP[A, B, AA, BB] = new LensP[A, B, AA, BB] {
    def apply[P[_, _]](f: P[A, B])(implicit prof: Cartesian[P]): P[AA, BB] = {
      prof.dimap(utils.fork(lens.view)(identity _))(lens.update)(prof.first(f))
    }
  }
}

object utils {
  def fork[X, Y, Z](f: X => Y)(g: X => Z)(x: X): Tuple2[Y, Z] = (f(x), g(x))

  def either[X, Y, Z](f: X => Z)(g: Y => Z)(xy: Either[X, Y]): Z = xy match {
    case Left(x) => f(x)
    case Right(y) => g(y)
  }

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

trait Prism[A, B, AA, BB] {
  def match2(aa: AA): Either[BB, A]
  def build(b: B): BB
}

trait PrismP[A, B, AA, BB] {
  def apply[P[_, _]](h: P[A, B])(implicit prof: Cocartesian[P]): P[AA, BB]
}

object Prism {
  def c2p[A, B, AA, BB](prism: Prism[A, B, AA, BB]): PrismP[A, B, AA, BB] = new PrismP[A, B, AA, BB] {
    def apply[P[_, _]](f: P[A, B])(implicit prof: Cocartesian[P]): P[AA, BB] = {
      prof.dimap(prism.match2)(utils.either((bb: BB) => bb)(prism.build _))(prof.right(f))
    }
  }
}

// https://twanvl.nl/blog/haskell/non-regular1
sealed trait FunList[A, B, T]
case class Done[A, B, T](t: T) extends FunList[A, B, T]
case class More[A, B, T](a: A, fl: FunList[A, B, B => T]) extends FunList[A, B, T]

trait Traversal[A, B, AA, BB] {
  def extract(aa: AA): FunList[A, B, BB]
}

trait TraversalP[A, B, AA, BB] {
  def apply[P[_, _]](h: P[A, B])(implicit cart: Cartesian[P], cocart: Cocartesian[P], monoid: Monoidal[P]): P[AA, BB]
}

object Traversal {
  def c2p[A, B, AA, BB](traversal: Traversal[A, B, AA, BB]): TraversalP[A, B, AA, BB] = new TraversalP[A, B, AA, BB] {
    def apply[P[_, _]](k: P[A, B])(implicit cart: Cartesian[P], cocart: Cocartesian[P], monoid: Monoidal[P]): P[AA, BB] = {
      val lmap: (AA => FunList[A, B, BB]) = traversal.extract
      val rmap: (FunList[B, B, BB] => BB) = utils.fuse _
      val mid: P[FunList[A, B, BB], FunList[B, B, BB]] = utils.traverse(k)

      cart.dimap(lmap)(rmap)(mid)
    }
  }
}
