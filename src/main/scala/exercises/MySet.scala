package exercises

import scala.annotation.tailrec
import scala.compiletime.ops.any

object ExerciseMySet extends App {
  val phi = MySet[Int]()
  val set1 = MySet(1)
  val set2 = MySet(2, 3)
  val set3 = set1 ++ set2 ++ set2 ++ set2
  val set4 = !set3

  println(s"phi: ${phi}")
  println(s"set2: ${set1}")
  println(s"set2: ${set2}")
  println(s"set3: ${set3}")
  println(s"set4: ${set4}")
  println(s"set4 + 2: ${set4 + 2}")
  println(s"set3.filter(x % 2 != 0): ${set3.filter(_ % 2 != 0)}")
  println(s"set3 difference set1: ${set3 ^ set1}")
  println(s"set3 difference set2: ${set3 ^ set2}")
  println(s"set2 difference set3: ${set2 ^ set3}")
  println(s"set3 difference set3: ${set3 ^ set3}")
  println(s"set3 difference set4: ${set3 ^ set4}")
  println(s"set3 difference phi: ${set3 ^ phi}")
  println(s"set1 intersection set2: ${set1 & set2}")
  println(s"set1 intersection set3: ${set1 & set3}")
  println(s"set2 intersection set3: ${set2 & set3}")
  println(s"set3 intersection set4: ${set3 & set4}")
  println(s"set3 intersection phi: ${set3 & phi}")
  println(s"set3 union phi: ${set3 | phi}")
  println(s"set3 union set4: ${set4 | set3}")
  println(s"phi union phi: ${phi | phi}")
  println(s"compliment of phi: ${!phi}")
  println(s"set3 to seq: ${set3.toSeq}")
}

val NoEligibleMethod = new IllegalArgumentException(
  "The method cannot be implimented"
)

trait CollectionsMethod[A] {
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(p: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

trait MySet[A] extends (A => Boolean) with CollectionsMethod[A] {
  // Collections
  infix def contains(elem: A): Boolean
  infix def +(elem: A): MySet[A]
  infix def ++(that: MySet[A]): MySet[A]

  // Set Operation
  infix def -(elem: A): MySet[A] = this.filter(_ != elem)
  infix def |(that: MySet[A]): MySet[A] = this ++ that
  infix def ^(that: MySet[A]): MySet[A] = this.filter(!that)
  infix def &(that: MySet[A]): MySet[A] = this.filter(that)
  def unary_! : MySet[A]
  // Partial function
  def apply(elem: A): Boolean = this.contains(elem)
  def isDefinedAt(elem: A): Boolean = this.contains(elem)

  def detach(): (Option[A], MySet[A]) 
  def toSeq: Seq[A]
}

object MySet {
  def apply[A](elems: A*): MySet[A] = {
    @tailrec
    def builder(elems: Seq[A], acc: MySet[A]): MySet[A] = {
      if (elems.isEmpty) acc
      else builder(elems.tail, acc + elems.head)
    }
    builder(elems, NullSet())
  }
  
}

class NullSet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new EnumSet[A](elem, this)
  def ++(that: MySet[A]): MySet[A] = that
  def toSeq: Seq[A] = Seq[A]()
  def map[B](f: A => B): MySet[B] = new NullSet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new NullSet[B]
  def filter(p: A => Boolean): MySet[A] = this
  def unary_! : MySet[A] = Universe[A]
  def foreach(f: A => Unit): Unit = ()
  def detach(): (Option[A], MySet[A]) = (None, this)
  override def toString(): String = "∅"
}

case class EnumSet[A](elem: A, tail: MySet[A]) extends MySet[A] {
  // MySet
  infix def contains(elem: A): Boolean = {
    elem == this.elem || this.tail.contains(elem)
  }
  infix def +(elem: A): MySet[A] = {
    if (this.contains(elem)) this
    else new EnumSet[A](elem, this)
  }
  infix def ++(that: MySet[A]): MySet[A] = {
    this.tail ++ that + this.elem
  }
  def toSeq: Seq[A] = {
    this.elem +: this.tail.toSeq
  }
  def map[B](f: A => B): MySet[B] = {
    this.tail.map(f) + f(this.elem)
  }
  def flatMap[B](f: A => MySet[B]): MySet[B] = {
    this.tail.flatMap(f) ++ f(this.elem)
  }
  def filter(p: A => Boolean): MySet[A] = {
    if (p(this.elem)) tail.filter(p) + this.elem
    else tail.filter(p)
  }
  def unary_! : MySet[A] = new ComplementEnumSet[A](this.elem, this.tail)
  def foreach(f: A => Unit): Unit = {
    f(this.elem)
    this.tail.foreach(f)
  }
  def detach(): (Option[A], MySet[A]) = (Some(this.elem), this.tail)
  override def toString(): String =
    this.toSeq.toString().replaceAll("^List\\(", "Set(")
}

class Universe[A] extends MySet[A] {
  def contains(elem: A): Boolean = true
  override def toString(): String = "U"
  def ++(that: MySet[A]): MySet[A] = this
  def +(elem: A): MySet[A] = this
  def toSeq: Seq[A] = Seq[A]()
  def map[B](f: A => B): MySet[B] = new Universe[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new Universe[B]
  def filter(p: A => Boolean): MySet[A] = this
  def unary_! : MySet[A] = NullSet[A]
  def detach(): (Option[A], MySet[A]) = (None, this)
  def foreach(f: A => Unit): Unit = ()
}

class ComplementEnumSet[A](elem: A, tail: MySet[A]) extends EnumSet[A](elem, tail) {
  override def contains(elem: A): Boolean = !super.contains(elem)  
  override def +(elem: A): MySet[A] = {
    val (e, s) = super.filter(_!=elem).detach()
    if (e.isEmpty) Universe[A]()
    else new ComplementEnumSet[A](e.get, s)
  }
  override def ++(that: MySet[A]): MySet[A] = {
    val (e, s) = super.filter(!that).detach()
    if (e.isEmpty) Universe[A]()
    else new ComplementEnumSet[A](e.get, s)
  }
  override def apply(elem: A): Boolean = this.contains(elem)
  override def unary_! : MySet[A] = NullSet() ++ this.tail + this.elem
  override def filter(p: A => Boolean): MySet[A] = {
    new RuleBasedSet[A](x => p(x) && this(x))
  }
  override def map[B](f: A => B): MySet[B] = throw NoEligibleMethod
  override def flatMap[B](f: A => MySet[B]): MySet[B] = throw NoEligibleMethod
  override def foreach(f: A => Unit): Unit = throw NoEligibleMethod
  override def toSeq: Seq[A] = {
    this.elem +: this.tail.toSeq
  }
  override def toString(): String = {
    this.toSeq.toString().replaceAll("^List\\(", "Set'(")
  }
}

class RuleBasedSet[A](p: A => Boolean) extends MySet[A] {
  def isEmpty: Boolean = throw NoEligibleMethod
  override def contains(elem: A): Boolean = this.p(elem)
  def +(elem: A): MySet[A] = {
    if (this.contains(elem)) this
    else new RuleBasedSet[A](x => this.p(x) || p.equals(elem))
  }
  def ++(that: MySet[A]): MySet[A] = {
    new RuleBasedSet[A](x => this.p(x) || that.contains(x))
  }
  def unary_! : MySet[A] = {
    new RuleBasedSet[A](x => !this.p(x))
  }
  override def filter(p: A => Boolean): MySet[A] = {
    new RuleBasedSet[A](x => p(x) && this(x))
  }
  override def detach(): (Option[A], MySet[A]) = throw NoEligibleMethod
  override def map[B](f: A => B): MySet[B] = throw NoEligibleMethod
  override def flatMap[B](f: A => MySet[B]): MySet[B] = throw NoEligibleMethod
  override def foreach(f: A => Unit): Unit = throw NoEligibleMethod
  override def toSeq: Seq[A] = throw NoEligibleMethod
}
