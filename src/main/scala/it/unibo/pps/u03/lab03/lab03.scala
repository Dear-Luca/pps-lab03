package it.unibo.pps.u03.lab03

import it.unibo.pps.u02.AlgebraicDataTypes.Person
import it.unibo.pps.u02.AlgebraicDataTypes.Person.Teacher
import u03.Optionals.Optional
import u03.Optionals.Optional.{Empty, Just}
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, distinct, flatMap}

import scala.annotation.tailrec

// TASK 1
object task1 {

  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(h, t) if n > 0 => skip(t)(n - 1)
    case Cons(h, t) => Cons(h, t)
    case _ => Nil()

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case (_, _) => Nil()

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
    case (Cons(h1, t1), _) => Cons(h1, concat(t1, s2))
    case (Nil(), Cons(h2, t2)) => Cons(h2, concat(s1, t2))
    case (_, _) => Nil()

  def reverse[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))
    case _ => Nil()

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  def min(s: Sequence[Int]): Optional[Int] = s match
    case Cons(h, t) => Optional.orElse(Optional.map(min(t))(n => Just(h.min(n))), Just(h))
    case _ => Empty()

  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => concat(Cons(h, Nil()), evenIndices(skip(t)(1)))
    case _ => Nil()

  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) if h == elem => true
    case Cons(_, t) => contains(t)(elem)
    case _ => false

  def distinct[A](s: Sequence[A]): Sequence[A] =
    def skipDuplicates(s1: Sequence[A], seen: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) if contains(seen)(h) => skipDuplicates(t, seen)
      case Cons(h, t) => Cons(h, skipDuplicates(t, Cons(h, seen)))
      case _ => Nil()

    skipDuplicates(s, Nil())

  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    def streak(s: Sequence[A], prev: A): Sequence[A] = s match
      case Cons(h, t) if h == prev => Cons(h, streak(t, h))
      case _ => Nil()

    @tailrec
    def dropStreak(s: Sequence[A], prev: A): Sequence[A] = s match
      case Cons(h, t) if h == prev => dropStreak(t, h)
      case _ => s

    s match
      case Cons(h, t) => Cons(streak(s, h), group(dropStreak(t, h)))
      case _ => Nil()

  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    def trueValues(s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) if pred(h) => Cons(h, trueValues(t))
      case Cons(h, t) => trueValues(t)
      case _ => Nil()

    def falseValues(s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) if !pred(h) => Cons(h, falseValues(t))
      case Cons(h, t) => falseValues(t)
      case _ => Nil()

    (trueValues(s), falseValues(s))

}

// TASK 2
object task2 {
  def es1(sequence: Sequence[Person]): Sequence[String] =
    flatMap(sequence)(p => p match
      case Teacher(n, c) => Cons(c, Nil())
      case _ => Nil()
    )

  @tailrec
  def foldLeft[A, B](sequence: Sequence[A])(acc: B)(operator: (B, A) => B): B = sequence match
    case Cons(h, t) => foldLeft(t)(operator(acc, h))(operator)
    case Nil() => acc

  def es3(sequence: Sequence[Person]): Int =
    foldLeft(distinct(es1(sequence)))(0)((acc, s) => acc + 1)
}

// TASK 3
object task3 {
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:
    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()

    def fill[A](n: Int)(a: A): Stream[A] = n match
      case n if n > 0 => cons(a, fill(n - 1)(a))
      case _ => Empty()

    def fibonacci(): Stream[Int] =
      def fib(first: Int, second: Int): Stream[Int] =
        cons(first, fib(second, first + second))

      fib(0, 1)

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
      case Cons(h, t) => cons(h(), interleave(s2, t()))
      case _ => s2

    def cycle[A](lst: Sequence[A]): Stream[A] =
      val s = lst

      def next(lst: Sequence[A]): Stream[A] = lst match
        case Sequence.Cons(h, t) => cons(h, next(t))
        case Sequence.Nil() => next(s)

      lst match
        case Nil() => Empty()
        case _ => next(lst)

}