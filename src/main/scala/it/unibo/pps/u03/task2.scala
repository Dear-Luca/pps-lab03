package it.unibo.pps.u03

import it.unibo.pps.u02.AlgebraicDataTypes.Person
import it.unibo.pps.u02.AlgebraicDataTypes.Person.{Student, Teacher}
import u03.Sequences.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*


object task2:
  def filterCoursesOfTeachers(sequence: Sequence[Person]): Sequence[String] =
    flatMap(sequence)(p => p match
      case Teacher(n, c) => Cons(c, Nil())
      case _ => Nil()
    )


  @main
  def es1(): Unit =
    val persons: Sequence[Person] = Cons(Teacher("Pippo", "Algebra"), Cons(Student("Pluto", 2000), Cons(Teacher("Mario", "Math"), Nil())))

    println(filterCoursesOfTeachers(persons))

  @main
  def es2(): Unit =
    println("ciao2")