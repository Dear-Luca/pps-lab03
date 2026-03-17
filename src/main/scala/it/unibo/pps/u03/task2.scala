package it.unibo.pps.u03

import it.unibo.pps.u02.AlgebraicDataTypes.Person
import it.unibo.pps.u02.AlgebraicDataTypes.Person.{Student, Teacher}
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*


object task2:

  def es1(sequence: Sequence[Person]): Sequence[String] =
    flatMap(sequence)(p => p match
      case Teacher(n, c) => Cons(c, Nil())
      case _ => Nil()
    )

  def foldLeft[A, B](sequence: Sequence[A])(acc: B)(operator: (B, A) => B): B = sequence match
    case Cons(h, t) => foldLeft(t)(operator(acc, h))(operator)
    case Nil() => acc

  @main
  def es2(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    println(foldLeft(lst)(0)(_ - _))
  
  @main
  def es3(): Unit =
    println("ciao")
    
