package exercises

import scala.annotation.tailrec
import lectures.Section03_Advanced_Functional_Programming.V14_CurryingAndPartiallyAppliedFuncs.n

object MyStreamTest extends App {
  val s0 = EmptyStream
  val natural = MyStream.from(1)(_+1)
  val fabonacci1 = MyStream.from((1, 1))((x, y) => (y, x+y)).map(_._1)

  def fabonacci(x :Int, y:Int): MyStream[Int] = {
    new Stream[Int](x, fabonacci(y, x+y))
  }
  val fabonacci2 = fabonacci(1, 1)
  
  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] = {
    if (numbers.isEmpty) numbers
    else new Stream[Int](numbers.head, eratosthenes(numbers.tail.filter(_ % numbers.head != 0)))
  }
  
  val primes_1 = MyStream.from((2, true, List[Int](2)))(
    (currVal, currValIsPrime, primesUntilCurrVal) => {
      val successor = currVal+1
      val isPrime = primesUntilCurrVal.map(p => (successor) % p != 0).reduce((x1, x2) => x1 && x2)
      val primes = if (isPrime) primesUntilCurrVal :+ (currVal+1) else primesUntilCurrVal
      (successor, isPrime, primes)
    })
    .filter(_._2)
    .map(_._1)
  val primes_2 = eratosthenes(MyStream.from(2)(_+1))

  println(s"the 1st natural number: ${natural.head}")
  println(s"the 2nd natural number: ${natural.tail.head}")
  println(s"the 1st element start from 0: ${(0 #:: natural).head}")
  println(s"the 2nd element start from 0: ${(0 #:: natural).tail.head}")
  val s1 = natural.take(3)
  val s2 = 4 #:: 5 #:: 6 #:: EmptyStream
  val s01 = s0 ++ s1
  val s12 = s1 ++ s2

  s2.foreach(println)

  println(s"s0: ${s0}")
  println(s"s1: ${s1.take(1)}")
  println(s"s1: ${s1.takeAsList(10)}")

  println(s"s2: ${s2}")
  println(s"s0 ++ s1: ${s01}")
  println(s"s1 ++ s2: ${s12}")

  println(natural.map(_*2).takeAsList(10))
  println(natural.flatMap(x => new Stream(x, new Stream(x+1, EmptyStream))).takeAsList(10))
  println(natural.filter(x => x % 7 == 0 && x % 3 == 0 && x % 5 == 0).takeAsList(10))
  // // println(s12.flatMap(x => Stream(s"A${x}", Stream(s"B${x}", EmptyStream))))

  println(s"fabonacci(10) : ${fabonacci1.take(10)}")
  println(s"fabonacci(10) : ${fabonacci2.take(10)}")
  println(s"primes(10)    : ${primes_1.take(30)}")
  println(s"primes(10)    : ${primes_2.take(30)}")
  // println(s12.take(2))
  // println(s12.takeAsList(10))
  // println(s12.filter(_%2==0))

  // var state = start
  //   def tail: Stream[A] = {
  //     println(s"call next state: ${state}")
  //     lazy val next = new Stream[A](state, EmptyStream)
  //     state = generator(state)
  //     next
  //   }

  // var state: Int = 1
  // def generator(x: Int): Int = x+1
  // def tail: Stream[Int] = {
  //   println(s"call next state: ${state}")
  //   lazy val next = new Stream[Int](state, EmptyStream)
  //   state = generator(state)
  //   next
  // }
  // println(s"state: ${state} next: ${tail}")
  // println(s"state: ${state} next: ${tail}")
  // println(s"state: ${state} next: ${tail}")

}

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](elem: B): MyStream[B] // prepend operation
  def ++[B >: A](that: => MyStream[B]): MyStream[B] // concatenate two stream

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(p: A => Boolean): MyStream[A]

  override def toString(): String = {
    @tailrec
    def helper(s: MyStream[A], acc: String): String = {
      if (!s.isEmpty) helper(s.tail, s"${if (acc == "[") acc else acc + " ,"}${s.head}")
      else acc
    }
    helper(this, "[") + "]"
  }
  def take(n: Int): MyStream[A] // takes the first n elements out of this stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = List[B]()): List[B] = {
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
  }
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = {
    new Stream[A](start, MyStream.from(generator(start))(generator))
  }
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new IndexOutOfBoundsException
  def tail: MyStream[Nothing] = this

  def #::[B >: Nothing](elem: B): MyStream[B] = new Stream[B](elem, this)
  def ++[B >: Nothing](that: => MyStream[B]): MyStream[B] = that

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(p: Nothing => Boolean): MyStream[Nothing] = this
  def take(n: Int): MyStream[Nothing] = this
}

class Stream[A](h: A, t: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false
  override def head: A = h
  override lazy val tail: MyStream[A] = t
  def #::[B >: A](elem: B): MyStream[B] = {
    new Stream[B](elem, this)
  }
  def ++[B >: A](that: => MyStream[B]): MyStream[B] = {
    new Stream[B](this.head, this.tail ++ that)
  }
  def foreach(f: A => Unit): Unit = {
    f(this.head)
    this.tail.foreach(f)
  }
  def map[B](f: A => B): MyStream[B] = {
    new Stream[B](f(this.head), this.tail.map(f))
  }
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = {
    f(this.head) ++ this.tail.flatMap(f)
  }
  def filter(p: A => Boolean): MyStream[A] = {
    if (p(this.head)) new Stream[A](this.head, this.tail.filter(p))
    else this.tail.filter(p)
  }
  def take(n: Int): MyStream[A] = {
    if (n == 0) EmptyStream
    else if (n == 1) new Stream[A](this.head, EmptyStream)
    else new Stream[A](this.head, this.tail.take(n-1)) 
  }
}
