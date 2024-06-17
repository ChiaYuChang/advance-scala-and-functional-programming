package lectures.Section02_A_Taste_of_Advanced_Scala

import lectures.Section02_A_Taste_of_Advanced_Scala.V06_AdvancePatternMatching.Person.unapply
import scala.compiletime.ops.boolean

object V06_AdvancePatternMatching extends App {
  val numbers = List(1)
  val desc = numbers match
    case head :: Nil => s"the only element is ${head}"
    case _           => "whatever"
  println(desc)

  // if cannot make it a case class
  class Person(val name: String, val age: Int)

  object Person {
    def unapply(p: Person): Option[(String, Int)] = {
      if (p.age < 20) None
      else Some(p.name, p.age)
    }

    def unapply(age: Int): Option[String] = {
      if (age < 20) Some("miner") else Some("major")
    }
  }

  val bob = new Person("Bob", 18)
  val greeting1 = bob match
    // actually nothing to do with the Person class!!
    case Person(name, age) =>
      s"Hi, my name is ${name}, and I am ${age} years old."
    case _ => "nothing matched"
  println(greeting1)

  val legalStatus = bob.age match
    case Person(status) => s"My legal status is ${status}."
    case _              => "nothing matched"
  println(legalStatus)

  trait IntegerProperty {
    def unapply(n: Int): Boolean
  }

  val SingleDigit: IntegerProperty = x => x > -10 && x < 10
  val EvenNumber: IntegerProperty = x => x % 2 == 0
  val OddNumber: IntegerProperty = x => x % 2 == 1

  val int: Int = 4
  val property = int match
    case SingleDigit() => "single digit"
    case EvenNumber()  => "an even number"
    case OddNumber()   => "an odd number"
  println(property)

  // infix patern
  // infix is necessary for scala 3
  infix case class Or[A, B](a: A, b: B)
  val either = Or(2, "two")
  val hdesc = either match
    case num Or str => s"${num} is writtern as ${str}"

  // decomposing sequences
  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] = {
      if (list == Empty) Some(Seq.empty)
      else {
        println(s"curr: ${list.head}")
        unapplySeq(list.tail).map(list.head +: _)
      }
    }
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A])
      extends MyList[A]

  List[MyList[Char]](
    Cons('a', Empty),
    Cons('a', Cons('b', Empty)),
    Cons('a', Cons('c', Cons('e', Empty))),
    Cons('a', Cons('c', Cons('e', Cons('g', Cons('i', Cons('k', Empty))))))
  ).foreach(mylist => {
    val decomposed = mylist match
      case MyList('a', 'b', _*) => "Start with a, b"
      case MyList('a', 'c', _*) => "Start with a, c"
      case MyList('a', _*)      => "Start with a"
      case _                    => "otherwise"
    println(s"decomposed: ${decomposed}")
  })

  // self define pattern match return value
  // classes with both isEmpty and get methods
  abstract class Wrapper[T] { 
    def isEmpty: Boolean = ???
    def get: T = ???
  }

  object PersonWrapper {
    def unapply(p: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty = false
      override def get: String = p.name
    }
  }

  val greeting2 = bob match
    // actually nothing to do with the Person class!!
    case PersonWrapper(name) =>
      s"${name} is a person."
    case _ => "nothing matched"
  println(greeting2)
  
}
