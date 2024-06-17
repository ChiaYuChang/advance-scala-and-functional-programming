package lectures.Section02_A_Taste_of_Advanced_Scala

object V05_DarkSugars extends App {
    // 1. method with single parameter
    def singleArgMethod(arg: Int): String = arg.toString()

    val description = singleArgMethod {
        // do something
        13
    }
    println(description)

    List(1, 2, 3).map { x =>
        x + 3
    }.foreach(println)

    // 2. single abstract method
    trait Action {
        def act(x: Int): Int
    }

    val anInstance = new Action {
        override def act(x: Int): Int = x+1
    }
    val anotherInstance: Action = (x: Int) => x+2

    abstract class AnAbstractType {
        def func1(): Int = 10
        def func2(x: Int): Unit
    }
    val anAbstractInstance = (x: Int) => println(x)

    // 3. :: and #::
    val nums = 1 :: 2 :: 3 :: List(4, 5) // List(4, 5).::(3).::(2).::(1)
    println(nums.toString())

    abstract class Stream[+T] {
        def -->:[S >: T](value: S): Stream[S]
    }

    class MyStream[+T](val data: T, tail: Stream[T]) extends Stream[T] {
        def -->:[S >: T](value: S): Stream[S] = {
            println(s"get data: ${value}")
            new MyStream[S](value, this)
        }
        override def toString(): String = {
            if (tail.toString() == "") s"${this.data}"
            else tail.toString() + " " + s"${this.data}"
        }
    }

    object MyStream extends Stream[Nothing] {
        def -->:[T >: Nothing](value: T): Stream[T] = {
            println(s"get data: ${value}")
            new MyStream[T](value, MyStream)
        }
        override def toString(): String = ""
    }

    val mystream = "Sam." -->: "am" -->: "I" -->: "Hi," -->: MyStream
    println(mystream)

    // 4. multi-word method
    class TeenGirl(name: String) {
        def `and then said`(gossip: String) = println(s"$name said $gossip") 
    }

    val mary = TeenGirl("Mary")
    mary `and then said` "Scala is so sweet!!"

    // 5. infix types
    infix case class Composite[A, B](x: A, y: B)
    val c1 = new (String Composite Int)("John", 10)
    val c2 = new Composite[String, Int]("John", 10)
    println(s"c1: $c1, c2: $c2 equal? ${c1 == c2}")

    infix abstract class -->[A, B] {
        infix def to(x: A): B
    }
    val string2int: String --> Int = x => x.toInt
    println(string2int to "2")

    // 6 update
    // used in mutable collections
    val anArray = Array(1, 2, 3)
    println(s"anArray: ${anArray.toSeq}")
    anArray(2) = 7
    println(s"anArray: ${anArray.toSeq}")

    // 7 setters and getters for mutable containers
    class Mutable {
        private var value: Int = 0
        def member:Int = this.value
        def member_=(value: Int): Unit = this.value = value
        override def toString(): String = {
            s"Mutable{value: (${this.value})"
        }
    }
    val aMutableContainer = new Mutable
    println(aMutableContainer)
    aMutableContainer.member = 41
    println(aMutableContainer)
}
