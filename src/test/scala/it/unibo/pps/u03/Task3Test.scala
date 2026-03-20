package it.unibo.pps.u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence.*
import u03.Streams.*
import u03.Streams.Stream.fibonacci

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
