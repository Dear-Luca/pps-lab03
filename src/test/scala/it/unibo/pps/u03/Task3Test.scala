package it.unibo.pps.u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence.*
import u03.Streams.*

class Task3Test:
  @Test def testTakeWhile() =
    val s = Stream.iterate(0)(_ + 1)
    val res = Stream.toList(Stream.takeWhile(s)(_ < 5))
    val expected = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expected, res)

  @Test def testFill() =
    val expected = Cons("a", Cons("a", Cons("a", Nil())))
    val res = Stream.toList(Stream.fill(3)("a"))
    assertEquals(expected, res)



