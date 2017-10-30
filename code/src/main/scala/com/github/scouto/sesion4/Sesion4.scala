package com.github.scouto.sesion4

import scala.annotation.tailrec

/**
  * Created by scouto.
  */
object Sesion4 extends App{


  //Exercise 2.1
  def fib(n: Int): BigInt = {

    @tailrec
    def loop (acc1: BigInt, acc2: BigInt, x: Int) : BigInt = {

      if (x == n) acc1 + acc2
      else loop (acc2, acc1+ acc2, x+1)

    }

    if (n<2) n
    else loop (0, 1, 2)
  }

  def isSorted[A](as: List[A])(implicit ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop (rest: List[A]): Boolean = {
      rest match {
        case Nil => true
        case h::Nil => true
        case h1::t if ordered(h1, t.head) => loop (t)
        case h1::t if !ordered(h1, t.head) => false
      }
    }
    loop (as)
  }


  def msort[T]( xs: List[T])(implicit less: (T, T) => Boolean): List[T] = {

    @annotation.tailrec
    def accsort (acc: List[T], pending: List[T]): List[T] ={
      pending match {
        case h::Nil => acc
        case h::t if less(h, t.head) && !isSorted(t.tail) => accsort(acc:::List(h):::List(t.head), t.tail)
        case h::t if less(h, t.head) && isSorted(t.tail) => acc:::pending
        case h::t if !less(h, t.head) => accsort(acc:::List(t.head):::List(h), t.tail)
        case _ => acc

      }

    }
    accsort(List(), xs)

  }


  def msort2[T](less: (T, T) => Boolean, list: List[T]): List[T] = {




    def go(left: List[T], right: List[T]): List[T] = {
      (left, right) match {
        case (left, Nil) => left
        case (Nil, right) => right
        case (h1::t1, h2::t2) =>
          if (less(h1, h2)) h1::go(t1, right)
          else h2::go(left, t2)

      }
    }

    val n = list.length / 2
    if (n == 0) list
    else {
      val (l, r) = list.splitAt(n)
      go(msort2(less, l), msort2(less, r))
    }



  }

}
