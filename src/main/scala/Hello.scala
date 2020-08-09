import implicits._

case class Person(name: String, age: Option[Int])

object Hello extends App {

  val as = List(1, 2, 3)
  val fs = List[Int => Int](
    _ + 10,
    _ * 10
  )

  println(Applicative.ap(fs)(as))
  // List(11, 12, 13, 10, 20, 30)

  // *** Lens ***

  val person = Person("Jan Kowalski", Some(32))

  val personNameLens = new Lens[String, String, Person, Person] {
    def view(person: Person): String = person.name
    def update(u: (String, Person)): Person = u._2.copy(name = u._1)
  }

  val personName = Lens.c2p(personNameLens)

  val personAgeLens = new Lens[Option[Int], Option[Int], Person, Person] {
    def view(person: Person): Option[Int] = person.age
    def update(u: (Option[Int], Person)): Person = u._2.copy(age = u._1)
  }

  val personAge = Lens.c2p(personAgeLens)

  def upperCaseString(s: String) = s.toUpperCase()
  val upperCaseName = personName(upperCaseString _)
  println(upperCaseName(person))
  // Person(JAN KOWALSKI,Some(32))

  // Not really a lens?
  val splitWordsLens = new Lens[List[String], List[String], String, String] {
    def view(s: String): List[String] = s.split(" ").toList
    def update(u: (List[String], String)): String = u._1.mkString(" ")
  }

  val splitWords = Lens.c2p(splitWordsLens)

  def reverseList(l: List[String]) = l.reverse
  val swapWords = splitWords(reverseList _)
  println(swapWords(person.name))
  // Kowalski Jan

  val swapNames = personName(splitWords(reverseList _))
  println(swapNames(person))
  // Person(Kowalski Jan,Some(32))

  // *** Prism ***

  val either12 = Left(12)

  val leftIntToBoolPrism = new Prism[Int, Boolean, Either[Int, String], Either[Boolean, String]] {
    def match2(aa: Either[Int, String]): Either[Either[Boolean, String], Int] = aa match {
      case Left(a) => Right(a)
      case Right(b) => Left(Right(b))
    }
    def build(b: Boolean): Either[Boolean, String] = Left(b)
  }

  val leftIntToBool = Prism.c2p(leftIntToBoolPrism)
  def isEven(i: Int) = i % 2 == 0
  val leftIsEven = leftIntToBool(isEven _)
  println(leftIsEven(either12))
  // Left(true)

  val optionIntToBoolPrism = new Prism[Int, Boolean, Option[Int], Option[Boolean]] {
    def match2(aa: Option[Int]): Either[Option[Boolean], Int] = aa match {
      case None => Left(None)
      case Some(a) => Right(a)
    }
    def build(b: Boolean): Option[Boolean] = Some(b)
  }

  val optionIntToBool = Prism.c2p(optionIntToBoolPrism)

  val optionIntPrism = new Prism[Int, Int, Option[Int], Option[Int]] {
    def match2(aa: Option[Int]): Either[Option[Int], Int] = aa match {
      case None => Left(None)
      case Some(a) => Right(a)
    }
    def build(b: Int): Option[Int] = Some(b)
  }

  val optionInt = Prism.c2p(optionIntPrism)

  val halveAge = personAge(optionInt((age: Int) => age / 2))
  println(halveAge(person))
  // Person(Jan Kowalski,Some(16))

  // *** Traversal ***

  val listFooBar = List("foo", "bar")

  val listElemsTraversal = new Traversal[String, String, List[String], List[String]] {
    def extract(aa: List[String]): FunList[String, String, List[String]] = aa match {
      // I don't have `Functor` and `Applicative` instances for `FunList` yet so let's cheat...
      case a :: Nil => More(a, Done((a1: String) => a1 :: Nil))
      case a :: b :: Nil => More(b, More(a, Done((b1: String) => (a1: String) => b1 :: a1 :: Nil)))
    }
  }

  val listElems = Traversal.c2p(listElemsTraversal)
  def enclose(s: String) = s"<$s>"
  val encloseStrings = listElems(enclose _)
  println(encloseStrings(listFooBar))
  // List(<foo>, <bar>)

  val encloseReverseNames = personName(splitWords(encloseStrings.compose(reverseList _)))
  println(encloseReverseNames(person))
  // Person(<Kowalski> <Jan>,Some(32))

  val tree = Node(5,
    Node(3, EmptyTree, EmptyTree),
    Node(8, EmptyTree, EmptyTree)
  )

  def list2(f: Int => List[Int]): (Int => Int => List[Int]) = (x: Int) => (y: Int) => x :: f(y)
  def list3(f: Int => Int => List[Int]): (Int => Int => Int => List[Int]) = (x: Int) => (y: Int) => (z: Int) => x :: f(y)(z)

  val three = More(3, Done[Int, Int, Int => List[Int]]((x: Int) => x :: Nil))
  val five = three.wrap(5)((f: Int => List[Int]) => (x: Int) => (y: Int) => list2(f)(x)(y))
  // val eight = five.wrap(8)((f: Int => (Int => List[Int])) => (x: Int) => ((y: Int) => ((z: Int) => list3(f)(x)(y)(z))))

  def curry[A, B, C, D](f: B => C)(g: A => (C => D)): A => (B => D) = (a: A) => (b: B) => g(a)(f(b))
  def append[T](t: T)(l: List[T]): List[T] = t :: l

  val clist2 = curry((i: Int) => i :: Nil)(append)
  // val clist3 = curry[Int, Int, Int => List[Int], Int => Int => List[Int]](Function.uncurried(clist2))(append)
}

sealed trait Tree[+T]
case object EmptyTree extends Tree[Any]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
