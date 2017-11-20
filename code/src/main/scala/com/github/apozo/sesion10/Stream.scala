package com.github.apozo.sesion10

/**
  * Created by couto.
  */
sealed trait Stream[+A] {

  def headOption: Option[A] = ???

  def toList: List[A] = ???

//  @tailrec
  final def drop(n: Int): Stream[A] = ???

//  @tailrec
  final def dropWhile(f: A => Boolean): Stream[A] = ???

  def take(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  //constructor
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  //constructor de empty Stream con ti'o
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  }


