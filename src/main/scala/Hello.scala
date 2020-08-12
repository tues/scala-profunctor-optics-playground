import implicits._

case class Person(name: String, age: Option[Int])

class TListFunList[X] {
  def ap[A, B] = FunList.funListApplicative[X, X].ap[A, B] _
  def cons[T](t: T): FunList[T, T, List[T] => List[T]] = More(t, Done((t1: T) => (l: List[T]) => t1 :: l))
}

object Hello extends App {
  println("")

  new LensPlayground()

  println("")
}

class FunListPlayground {
  val slfl = new TListFunList[String]
  import slfl._

  // val pfl: FunList[String, String, List[String] => List[String]] = Done((l: List[String]) => l.map(_.reverse))
  // val pfl2: FunList[String, String, List[String] => List[String]] = More("baz", Done((s: String) => (l: List[String]) => s :: l))
  // val sfl1: FunList[String, String, List[String]] = More("foo", Done((a1: String) => a1 :: Nil))

  val fl0: FunList[String, String, List[String]] = Done(Nil)
  val fl1 = ap(cons("foo"))(fl0)
  val fl2 = ap(cons("bar"))(fl1)
  val fl3 = ap(cons("baz"))(fl2)
  val fl4 = ap(cons("fnord"))(fl3)
  println(FunList.fuse(fl4))
  // List(fnord, baz, bar, foo)
}

class LensPlayground {
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

  val splitCharsLens = new Lens[List[String], List[String], String, String] {
    def view(s: String): List[String] = s.map(_.toString).toList
    def update(u: (List[String], String)): String = u._1.mkString(" ")
  }

  val splitChars = Lens.c2p(splitCharsLens)

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

  val listFooBar = List("foo", "bar", "baz", "fnord")

  val listElemsTraversal = new Traversal[String, String, List[String], List[String]] {
    val slfl = new TListFunList[String]
    import slfl._

    def extract(aa: List[String]): FunList[String, String, List[String]] = aa match {
      case Nil => Done(Nil)
      case a :: tail => ap(cons(a))(extract(tail))
    }
  }

  val listElems = Traversal.c2p(listElemsTraversal)

  def listElemsTraversalT[T] = new Traversal[T, T, List[T], List[T]] {
    val tlfl = new TListFunList[T]
    import tlfl._

    def extract(aa: List[T]): FunList[T, T, List[T]] = aa match {
      case Nil => Done(Nil)
      case a :: tail => ap(cons(a))(extract(tail))
    }
  }

  def listElemsT[T] = Traversal.c2p(listElemsTraversalT[T])

  def enclose(s: String) = s"<$s>"
  val encloseStrings = listElems(enclose _)
  println(encloseStrings(listFooBar))
  // List(<foo>, <bar>, <baz>, <fnord>)

  val encloseReverseNames = personName(splitChars(encloseStrings.compose(reverseList _)))
  println(encloseReverseNames(person))
  // Person(<Kowalski> <Jan>,Some(32))

  val enclosePeopleNames = listElemsT[Person](personName(enclose _))

  val people = List(person, Person("Grzegorz Brzęczyszczykiewicz", None))
  println(enclosePeopleNames(people))
  // List(Person(<Jan Kowalski>,Some(32)), Person(<Grzegorz Brzęczyszczykiewicz>,None))
}

sealed trait Tree[+T]
case object EmptyTree extends Tree[Any]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

object TreePlayground {
  val tree = Node(5,
    Node(3, EmptyTree, EmptyTree),
    Node(8, EmptyTree, EmptyTree)
  )
}
