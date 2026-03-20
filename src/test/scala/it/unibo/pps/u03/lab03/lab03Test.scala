package it.unibo.pps.u03.lab03

import it.unibo.pps.u02.AlgebraicDataTypes.Person
import it.unibo.pps.u02.AlgebraicDataTypes.Person.{Student, Teacher}
import it.unibo.pps.u03.task2.{es1, es3, foldLeft}
import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil}
import u03.Streams.Stream
import u03.Streams.Stream.fibonacci

class task2Test:
  val persons: Sequence[Person] = Cons(Teacher("Pippo", "Algebra"), Cons(Student("Pluto", 2000), Cons(Teacher("Mario", "Math"), Cons(Teacher("Luca", "Math"), Nil()))))
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testEs1() =
    assertEquals(Cons("Algebra", Cons("Math", Cons("Math", Nil()))), es1(persons))

  @Test def testEs2() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(0, foldLeft(lst)(0)(_ / _))
    assertEquals(16, foldLeft(lst)(0)(_ + _))

  @Test def testEs3() =
    assertEquals(2, es3(persons))
    assertEquals(0, es3(Nil()))

class task3Test:
  @Test def testTakeWhile() =
    val s = Stream.iterate(0)(_ + 1)
    val res = Stream.toList(Stream.takeWhile(s)(_ < 5))
    val expected = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expected, res)

  @Test def testFill() =
    val expected = Cons("a", Cons("a", Cons("a", Nil())))
    val res = Stream.toList(Stream.fill(3)("a"))
    assertEquals(expected, res)


  @Test def testFibonacci() =
    val res = Stream.toList(Stream.take(fibonacci())(5))
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    assertEquals(expected, res)

  @Test def testInterleave() =
    val s1 = Stream.take(Stream.iterate(1)(_ + 1))(3)
    val s2 = Stream.take(Stream.iterate(1)(_ + 1))(2)
    val res1 = Stream.toList(Stream.interleave(s1, s2))
    val expected = Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Nil())))))
    val res2 = Stream.toList(Stream.interleave(s2, s1))
    assertEquals(expected, res1)
    assertEquals(expected, res2)

  @Test def testCycle() =
    val s1 = Stream.cycle(Cons("a", Cons("b", Cons("c", Nil()))))
    val res1 = Stream.toList(Stream.take(s1)(7))
    val expected1 = Cons("a", Cons("b", Cons("c", Cons("a", Cons("b", Cons("c", Cons("a", Nil())))))))
    val s2 = Stream.cycle(Nil())
    val res2 = Stream.toList(Stream.take(s2)(3))
    val expected2 = Nil()
    assertEquals(expected2, res2)
    assertEquals(expected1, res1)

