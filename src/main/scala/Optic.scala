trait Lens[A, B, AA, BB] {
  def view(aa: AA): A
  def update(baa: (B, AA)): BB
}

trait LensP[A, B, AA, BB] {
  def apply[P[_, _]](h: P[A, B])(implicit prof: Cartesian[P]): P[AA, BB]
}

object Lens {
  // AA = S
  // BB = T
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
}

trait Prism[A, B, AA, BB] {
  def match2(aa: AA): Either[BB, A] // match
  def build(b: B): BB
}

trait PrismP[A, B, AA, BB] {
  def apply[P[_, _]](h: P[A, B])(implicit prof: Cocartesian[P]): P[AA, BB]
}

object Prism {
  def c2p[A, B, AA, BB](prism: Prism[A, B, AA, BB]): PrismP[A, B, AA, BB] = new PrismP[A, B, AA, BB] {
    def apply[P[_, _]](f: P[A, B])(implicit prof: Cocartesian[P]): P[AA, BB] = {
      prof.dimap(prism.match2)(utils.either(identity[BB])(prism.build _))(prof.right(f))
    }
  }
}

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
      val rmap: (FunList[B, B, BB] => BB) = FunList.fuse _
      val mid: P[FunList[A, B, BB], FunList[B, B, BB]] = FunList.traverse(k)

      cart.dimap(lmap)(rmap)(mid)
    }
  }
}
