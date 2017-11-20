package com.github.apozo.sesion10

import com.github.apozo.sesion10.Stream._

/**
  * Created by couto.
  */
sealed trait Stream[+A] {

  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
  }

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  //  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  //  @tailrec
  final def dropWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if f(h()) => t().dropWhile(f)
      case _ => this
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => Stream(h())
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }
  }
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


