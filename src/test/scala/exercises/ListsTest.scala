package exercises

import org.junit.Test
import exercises.Lists.*
import exercises.Lists.List.*
import org.junit.Assert.assertEquals
import u02.Modules.Person
import u02.Optionals.*

class ListsTest {

  private val l1 = Cons(10, Cons(20, Cons(30, Nil ())))
  private val l2 = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test
  def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l1, 1))
    assertEquals(Cons(30, Nil()), drop(l1, 2))
    assertEquals(Nil(), drop(l1, 5))

  @Test
  def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l1, tail))
    assertEquals(Cons(40, Cons(10, Cons(20, Cons(30, Nil ())))), append(tail, l1))

  @Test
  def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l1)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l1)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test
  def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None, max(Nil()))

  @Test
  def testMapWithFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l1)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l1)(_ + ""))

  @Test def testFilterWithFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l1)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l1)(_ != 20))

  @Test
  def testGetCourses() =
    val teacher = Cons(Person.Teacher("Maria", "LCMC"), Cons(Person.Teacher("Luca", "Web"), Nil()))
    assertEquals(Cons("LCMC", Cons("Web", Nil())), getCourses(teacher))

  @Test
  def testFoldLeft() =
    assertEquals(-16, foldLeft(l2)(0)(_ - _))

  @Test
  def testFoldRight() =
    assertEquals(-8, foldRight(l2)(0)(_ - _))
}