package exercises

import scala.annotation.tailrec
import u02.Modules.Person
import u02.Modules.Person.*

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case Cons(h, t) => Cons(h, t)
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, t), right) => Cons(h, append(t, right))
      case (Nil(), right) => right
      case (left, Nil()) => left

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case Some(a) if a > h => Some(a)
        case _ => Some(h)
      case Nil() => None

    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)({case i => Cons(mapper(i), Nil()); case _ => Nil()})

    def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)({case u if pred(u) => Cons(u, Nil()); case _ => Nil()})

    def getCourses(l: List[Person]): List[String] = flatMap(l)({case Teacher(n, c) => Cons(c, Nil()); case _ => Nil()})

    def foldLeft[A, B](l: List[A])(acc: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
      case Nil() => acc

    def foldRight[A, B](l: List[A])(acc: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight(t)(acc)(f))
      case Nil() => acc