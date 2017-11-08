package com.github.apozo.sesion5

import scala.annotation.tailrec

/**
  * Created by apozo.
  */
sealed trait Lista[+A]

case object Vacio extends Lista[Nothing]

case class Cons[A](head: A, tail: Lista[A]) extends Lista[A]


object Lista {


  def apply[A](as: A*): Lista[A] =
    if (as.isEmpty) Vacio
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: Lista[Int]): Int = {
    ints match {
      case Cons(h, t) => sum(t) + h
      case Vacio => 0
    }
  }

  def product(ints: Lista[Double]): Double = {
    ints match {
      case Vacio => 1
      case Cons(h, t) => h * product(t)
    }
  }

  def tail[A](list: Lista[A]): Lista[A] = {
    list match {
      case Vacio => Vacio
      case Cons(h, t) => t
    }
  }

  def setHead[A](list: Lista[A], newHead: A): Lista[A] = {
    list match {
      case Vacio => Lista(newHead)
      case Cons(h, t) => Cons(newHead, t)
    }
  }

  //List(1,2,3,4,5) drop(3) List(4,5)
  def drop[A](list: Lista[A], n: Int): Lista[A] = {
    @tailrec
    def delete(remainList: Lista[A], index: Int): Lista[A] = {
      (list, index) match {
        case (Vacio, _) => list
        case (Cons(h, t), current) if current <= 0 => remainList
        case (Cons(h, t), current) if current > 0 => delete(t, index - 1)
      }
    }
    delete(list, 0)
  }

  def dropWhile[A](list: Lista[A])( f: A => Boolean): Lista[A] = {
    @tailrec
    def delete(remainList: Lista[A]): Lista[A] = {
      list match {
        case Vacio => Vacio
        case Cons(h, t) if f(h) => delete(remainList)
        case Cons(h, t) if !f(h) => remainList
      }
    }
    delete(list)
  }

  def append[A](l1: Lista[A], l2: Lista[A]): Lista[A] = {

    l1 match {
      case Vacio => l2
      case Cons(h, t) => Cons(h, append(t, l2))
    }
  }


  //Lista(1,2,3,4) debería ser List(1,2,3)
  def init[A](l: Lista[A]): Lista[A] = {
    @tailrec
    def loop(acc: Lista[A], rest: Lista[A]): Lista[A] = {
      rest match {
        case Vacio => acc
        case Cons(h, Vacio) => acc
        case Cons(h, t) => loop(append(acc, Lista(h)), t)
      }
    }
    loop(Lista(), l)
  }

}



