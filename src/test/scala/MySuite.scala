// For more information on writing tests, see
import exercises.{MySet, NullSet, EnumSet}

// https://scalameta.org/munit/docs/getting-started.html
class TestMySet extends munit.FunSuite {
  val set1 = MySet(1)
  val set2 = MySet(2, 3)
  val set3 = set1 ++ set2 

  test("set1 OK") {
    val obtained = set1.contains(1)
    val expected = true
    assertEquals(obtained, expected)
  }

  test("set1 Not found") {
    val obtained = set1.contains(2)
    val expected = false
    assertEquals(obtained, expected)
  }

  test("set2 OK") {
    val obtained = set2.contains(3)
    val expected = true
    assertEquals(obtained, expected)
  }

  test("set2 Not found") {
    val obtained = set2.contains(1)
    val expected = false
    assertEquals(obtained, expected)
  }

  test("set3 OK") {
    val obtained = (1 to 3).map(set3.contains).reduce(_&&_)
    val expected = true
    assertEquals(obtained, expected)
  }

  test("set3 Not found") {
    val obtained = set3.contains(4)
    val expected = false
    assertEquals(obtained, expected)
  }

  test("set3 To String Set OK") {
    val obtained = set3.map(_.toString()).contains("1")
    val expected = true
    assertEquals(obtained, expected)
  }

  test("set3 To String Set Not found") {
    val obtained = set3.map(_.toString()).contains("4")
    val expected = false
    assertEquals(obtained, expected)
  }

  test("set3 flatMap negative OK") {
    val obtained = set3.flatMap(x => (new NullSet[Int]) + x + (-x)).contains(-1)
    val expected = true
    assertEquals(obtained, expected)
  }

  test("set3 flatMap negative Not found") {
    val obtained = set3.flatMap(x => (new NullSet[Int]) + x + (-x)).contains(0)
    val expected = false
    assertEquals(obtained, expected)
  }
  
  test("set3 filter OK") {
    val obtained = set3.filter(_ <= 1).contains(1)
    val expected = true
    assertEquals(obtained, expected)
  }

  test("set3 filter Not found") {
    val obtained = set3.filter(_ <= 1).contains(2)
    val expected = false
    assertEquals(obtained, expected)
  }

  test("set3 foreach OK") {
    val obtained = set3.foreach(println)
    val expected = ()
    assertEquals(obtained, expected)
  }
}
