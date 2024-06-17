package lectures.Section03_Advanced_Functional_Programming

import scala.compiletime.ops.boolean

object V15_LazyEvaluation extends App {
  lazy val err = throw new RuntimeException
  lazy val x = {
    println("Constructing x")
    10
  }

  println("Calling x")
  println(x)
  println(x) // x has been set to 10, do not need to reconstruct

  def funcSideEffect: Boolean = {
    println("inside func")
    true
  }
  def funcSimple: Boolean = false
  lazy val lazyFunc = funcSideEffect
  // lazyFunc not compute since funcSimple is false
  println(if(funcSimple && lazyFunc) "yes" else "no") 

  def byNameMethod(n: => Int): Int = n + n + n + 1
  def callbyNeedMethod(n: => Int): Int = {
    lazy val t = longComputeFunc 
    byNameMethod(t)
  }

  def longComputeFunc: Int = {
    print("Computing....")
    Thread.sleep(200)
    println("Done")
    12
  }

  println(s"result: ${byNameMethod(longComputeFunc)}")
  println(s"result: ${callbyNeedMethod(longComputeFunc)}")

  def lt30filter(i: Int): Boolean = {
    println(s"Is ${i} lt 30? ${if (i < 30) "Yes" else "No"}")
    i < 30
  }

  def gt20filter(i: Int): Boolean = {
    println(s"Is ${i} gt 20? ${if (i > 20) "Yes" else "No"}")
    i > 20
  }

  val numbers = List[Int](1, 10, 25, 30, 26, 40, 21)
  println("normal")
  val lt30 = numbers.filter(lt30filter)
  val gt20 = lt30.filter(gt20filter)
  println(s"normal: ${gt20}")

  println("lazy")
  // no side effect is applied -> filtering actions do not take place
  lazy val lazylt30 = numbers.withFilter(lt30filter)
  lazy val lazygt20 = lazylt30.withFilter(gt20filter)
  lazygt20.foreach(x => println(s"x = ${x}"))

  // for-comprehensions use withFilter
  for {
    a <- List[Int](1, 2, 3) if a % 2 == 0 // use lazy vals
  } yield println(a + 1)
  // -> List[Int](1, 2, 3).withFilter(_%2 == 0).map(x => println(x+1))
}
