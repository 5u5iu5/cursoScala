package com.github.apozo.sesion11

import scala.annotation.tailrec
import com.github.apozo.sesion11.Stream._

/**
  * Created by couto.
  */
sealed trait Stream[+A] {

  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
  }


  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h,t) => h() :: t().toList
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    //    @tailrec
    //    def loop(rest: Stream[A], x: Int): Stream[A] = {
    //      rest match {
    //        case Cons(h, t) if x > 0  => loop(t(), x-1)
    //        case _ => rest
    //      }
    //    }
    //    loop(this, n)

  }

  @tailrec
  final def dropWhile(f: A => Boolean): Stream[A] = {

    this match {
      case Cons(h, t) if f(h()) => t().dropWhile(f)
      case _ => this
    }

    //    @tailrec
    //    def go(rest: Stream[A]): Stream[A] = {
    //      rest match {
    //        case Empty => Empty
    //        case Cons(h, t) if f(h()) => go(t())
    //        case Cons(h, _) if !f(h()) => rest
    //      }
    //    }
    //
    //    go(this)
  }

  def take(n: Int): Stream[A] = {
    this match {
      //case Cons(h, t) if n > 1 => Cons(() => h(), () => t().take(n - 1))
      case Cons(h, t) if n > 1 => cons( h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream(h())
      case _ => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  //true si algun elemento cumple
  def exists(f: A => Boolean): Boolean = ???

  //Sesion 11
  def foldRight[B](z: => B)(f: (A, => B) => B): B = ???

  //  @tailrec
  final def foldLeft[B](z: => B)(f: ( => B, A) => B): B = ???

  def existsFoldRight(f: A => Boolean): Boolean = ???

  def existsFoldLeft(f: A => Boolean): Boolean = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOptionFold: Option[A] = ???

  def takeWhileFold(p: A => Boolean): Stream[A] = ???

  def map[B](f: A => B): Stream[B] = ???

  def filter(p: A => Boolean): Stream[A] = ???


  def append[B >: A](other: => Stream[B]): Stream[B] = ???

  def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  def find(p: A => Boolean): Option[A] = ???

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

  //constructor de empty Stream con tipo
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  //Sesion 11

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = ???

  def from(n: Int): Stream[Int] = ???

  def fibs: Stream[Int] = ???

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = ???

  def onesUnfold: Stream[Int] = ???

  def constantUnfold[A](a: A): Stream[A] = ???

  def fromUnfold(n: Int): Stream[Int] = ???

  def fibsUnfold: Stream[Int] = ???

}


