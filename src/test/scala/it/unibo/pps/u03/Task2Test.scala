package it.unibo.pps.u03

import it.unibo.pps.u02.AlgebraicDataTypes.Person
import it.unibo.pps.u02.AlgebraicDataTypes.Person.{Student, Teacher}
import it.unibo.pps.u03.task2.{es1, es3, foldLeft}
import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil}

class Task2Test:
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