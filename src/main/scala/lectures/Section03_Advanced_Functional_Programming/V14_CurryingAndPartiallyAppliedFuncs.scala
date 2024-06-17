package lectures.Section03_Advanced_Functional_Programming

object V14_CurryingAndPartiallyAppliedFuncs extends App {
  // Eta(η) Expansion (lifting methods to functions)
  // def add(x: Int, y: Int) = x + y
  // val (x: Int, y: Int) => Int = (x: Int, y: Int) = add(x, y)
  // ==> Eta Expansion           = add

  def curriedAdder(x: Int)(y : Int): Int = x + y

  // curriedAdder(3)(y: Int): Int = 3 + y
  // in scala2 val add3: Int => Int = curriedAdder(3)
  //           val add3 = curriedAdder(3) _ // force conduct eta expansion, not support in scala3
  // in scala3 val add3 = curriedAdder(3) is ok
  val x = 3
  val y = 2
  val add3 = curriedAdder(x)
  println(s"$x + $y = ${add3(y)}")

  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int): Int = x + y
  def curriedAddMethod(x: Int)(y: Int): Int = x + y

  val n = 7
  val add7_0 = simpleAddFunction(_: Int, n)
  val add7_1 = (x: Int) => simpleAddFunction(x, n)
  val add7_2 = (x: Int) => simpleAddFunction.curried(n)
  
  val add7_3 = (x: Int) => simpleAddMethod.curried(n)
  val add7_4 = x => simpleAddMethod(x, n)
  val add7_5 = simpleAddMethod(_: Int, n)

  val add7_6 = (x: Int) => curriedAddMethod(7)(x)
  val add7_7 = curriedAddMethod(7)

  def concatenator(a: String, b: String, c: String): String = s"${a} ${b} ${c}"
  val greeting_0 = concatenator("Hello, I'm", _: String, "How are you?")
  println(greeting_0("Sam."))

  val greeting_1 = concatenator("Hello, I'm", _: String, _: String)
  println(greeting_1("Jan.", "I'm learning scala3."))
  
  println(s"formatting ${math.Pi}")
  List[String]("%%4.2f", "%%8.6f", "%%14.12f")
    .map(s => f"${s}%8s: ${s}")
    .map(fmt => (x: Double) => fmt.format(x))
    .map(func => func(math.Pi))
    .foreach(println)

  def byName(n: => Int): Int = n+1
  def byFunc(f: () => Int): Int = f() + 1

  def m1: Int = 43
  def m2(): Int = 43

  println("byName needs an Int val")
  println(s"  - byName(43)       = ${byName(43)}")
  println(s"  - byName(m1)       = ${byName(m1)}")
  println(s"  - byName(m2())     = ${byName(m2())}")
  println(s"  - byName(m2)       = retuns error in scala3")
  println(s"  - byName(() => m1) = retuns error")

  println("byFunc needs a Func w/o parameter and return an Int val")
  println(s"  - byFunc(() => m1) = ${byFunc(() => m1)}")
  println(s"  - byFunc(m2)       = ${byFunc(m2)}")  
}
