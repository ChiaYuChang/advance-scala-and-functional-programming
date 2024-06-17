package lectures.Section03_Advanced_Functional_Programming

import scala.annotation.switch
import scala.util.Random

object V09_PartialFunctions extends App {
  val aFunc = (x: Int) => x + 1

  val aFussyFunc1 = (x: Int) => {
    if (x == 1) Some("x is one")
    else if (x == 3) Some("x is three")
    else if (x == 10) Some("x is ten")
    else None
  }
  (1 to 10)
    .map(aFussyFunc1)
    .filter(x => !x.isEmpty)
    .foreach(x => println(x.get))

  val aFussyFunc2 = (x: Int) => {
    x match
      case 1  => Some("x is one")
      case 3  => Some("x is three")
      case 10 => Some("x is ten")
      case _  => None
  }
  (1 to 10)
    .map(aFussyFunc2)
    .filter(x => !x.isEmpty)
    .foreach(x => println(x.get))

  // partial function
  // A partial function could only have one parameter
  val aPartialFunc: PartialFunction[Int, String] = {
    case 1  => "x is one"
    case 3  => "x is three"
    case 10 => "x is ten"
  }
  (1 to 10)
    .map(x => if (aPartialFunc.isDefinedAt(x)) aPartialFunc(x) else "")
    .filter(_ != "")
    .foreach(println)

  (1 to 10)
    .map(x => (x, aPartialFunc.isDefinedAt(x)))
    .foreach(x =>
      println(
        f"Is aPartialFunc defined at ${x._1}%2d? ${if (x._2) "Yes" else "No"}"
      )
    )

  (1 to 10)
    .map(x => aPartialFunc.lift(x))
    .filter(x => !x.isEmpty)
    .foreach(x => println(x.get))

  val aPartialFuncChain = aPartialFunc.orElse[Int, String] {
    case 9 => "x is nine"
    case 7 => "x is seven"
  }
  (1 to 10)
    .map(x => (x, aPartialFuncChain.isDefinedAt(x)))
    .foreach(x =>
      println(
        f"Is aPartialFuncChain defined at ${x._1}%2d? ${
            if (x._2) "Yes" else "No"
          }"
      )
    )

  // Partial functions extent normal functions
  // -> Partial functions are a subset of normal functions
  val aFussyFunc3: Int => Option[String] = {
    case 1      => Some("x is one")
    case 3      => Some("x is three")
    case 10     => Some("x is ten")
    case _: Int => None
  }
  (1 to 10)
    .map(aFussyFunc3)
    .filter(x => !x.isEmpty)
    .foreach(x => println(x.get))

  val aFussyFunc4 = new PartialFunction[Int, Option[String]] {
    def apply(x: Int): Option[String] = {
      x match
        case 1      => Some("x is one")
        case 3      => Some("x is three")
        case 10     => Some("x is ten")
        case _: Int => None
    }
    def isDefinedAt(x: Int): Boolean = {
      x == 1 || x == 3 || x == 10
    }
  }
  (1 to 10)
    .map(aFussyFunc4)
    .filter(x => !x.isEmpty)
    .foreach(x => println(x.get))

  val chatbot: PartialFunction[String, String] = {
    case "hello"   => "Hi, how are you."
    case "goodbye" => "Once your start talking to me, there is no return."
    case _: String => "I don't understand what you said."
  }

  scala.io.Source.stdin
    .getLines()
    .map(x =>
      println(s"input: ${x}")
      chatbot(x)
    )
    .foreach(println)
}
