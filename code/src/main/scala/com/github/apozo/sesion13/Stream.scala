package com.github.apozo.sesion13

import scala.annotation.tailrec
import com.github.apozo.sesion13.Stream._
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
  def exists(f: A => Boolean): Boolean = {
    this match {
      case Cons(h,t) => f(h()) || t().exists(f)
      case _ => false
    }

  }

  //Sesion 11
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  @tailrec
  final def foldLeft[B](z: => B)(f: ( => B, A) => B): B = {
    this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z

    }
  }

  def existsFoldRight(f: A => Boolean): Boolean = {
    foldRight(false)((elem, acc) => f(elem) || acc)
  }

  def existsFoldLeft(f: A => Boolean): Boolean = {
    foldLeft(false)((acc, elem) => f(elem) || acc)

  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((elem, acc) => p(elem) && acc)
  }

  def headOptionFold: Option[A] = {
    foldRight(None: Option[A])((elem, _) => Some(elem))
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((elem, acc) => if (p(elem)) cons(elem, acc) else empty)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((elem, acc) => cons(f(elem), acc))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((elem, acc) => if (p(elem)) cons(elem, acc) else acc)
  }

  def append[B >: A](other: => Stream[B]): Stream[B] = {
    foldRight(other)((elem, acc) => cons(elem, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((elem, acc) => f(elem) append acc)
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p) headOption
  }


  //Sesion 12
  def mapUnfold[B](f: A => B): Stream[B] = ???

  def takeUnfold(n: Int): Stream[A] = ???

  def takeWhileUnfold(p: A => Boolean): Stream[A] = ???

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = ???

  def zipWithAll[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C) : Stream[C] = ???


  def tails: Stream[Stream[A]] = ???

  //Sesion 13
  def zip[B](s2: Stream[B]): Stream[(A, B)] = ???

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

  def empiezaPor[A](s: Stream[A]): Boolean = ???

  def tieneSubsecuencia[A](s: Stream[A]): Boolean = ???


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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))


  //0,1,1, 2,3,5,8,13....
  def fibs: Stream[Int] = {
    def loop(acc1: Int, acc2: Int): Stream[Int] = {
      cons(acc1, loop(acc2, acc1+acc2))
    }
    loop(0,1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((value, state))  => cons(value, unfold(state)(f))
      //      case Some(tupla)  => cons(tupla._1, unfold(tupla._2)(f))
    }
  }

  def onesUnfold: Stream[Int] = {
    unfold(1){_ => Some((1,5))}
  }

  def constantUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }

  def fromUnfold(n: Int): Stream[Int] = {
    unfold(n)(a => Some(a, a+1))

  }

  def fibsUnfold: Stream[Int] = {
    //    unfold((0,1))(t => Some(t._1, (t._2, t._1+ t._2)))
    unfold((0,1)) {case (a,b) => Some(a, (b, a+b))}
  }

}


