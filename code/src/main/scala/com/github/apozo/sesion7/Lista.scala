package com.github.apozo.sesion7


import scala.annotation.tailrec

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

  def dropWhile[A](list: Lista[A])(f: A => Boolean): Lista[A] = {
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

  // -------------------- Sesion 6 --------------------

  def foldRight[A, B](as: Lista[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Vacio => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def sumFold(ints: Lista[Int]): Int = {
    foldRight(ints, 0)((a, b) => a + b)
  }

  def productFold(ints: Lista[Double]): Double = {
    foldRight(ints, 1d)((a, b) => a * b)
  }

  def length[A](as: Lista[A]): Int = {
    foldRight(as, 0)((a, acc) => acc + 1)
  }

  @tailrec
  def foldLeft[A, B](as: Lista[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Vacio => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumFoldLeft(ints: Lista[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def productFoldLeft(ints: Lista[Double]): Double = {
    foldLeft(ints, 1d)(_ * _)
  }

  def lengthFoldLeft[A](as: Lista[A]): Int = {
    foldLeft(as, 0)((acc, a) => acc + 1)
  }

  def reverse[A](as: Lista[A]): Lista[A] = {
    //foldLeft(as, Lista[A]())((acc, h) => Cons(h, acc))
    foldRight(as, Lista[A]())((h, acc) => append(acc, Lista(h)))
  }

  def foldRightbyLeft[A, B](as: Lista[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((acc, h) => f(h, acc))
  }

  def foldLeftbyRight[A, B](as: Lista[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((h, acc) => f(acc, h))
  }

  def productFoldRightLeft(ints: Lista[Double]): Double = {
    foldRightbyLeft(ints, 1.0)((h, t) => h * t)
  }

  def productFoldLeftRight(ints: Lista[Double]): Double = {
    foldLeftbyRight(ints, 1.0)((h, t) => h * t)
  }

  def lengthLeftRight[A](as: Lista[A]): Int = {
    foldLeftbyRight(as, 0)((acc, h) => acc + 1)
  }

  def lengthRightLeft[A](as: Lista[A]): Int = {
    foldRightbyLeft(as, 0)((h, acc) => acc + 1)
  }

  def appendFoldRight[A](a1: Lista[A], a2: Lista[A]): Lista[A] = {
    foldRight(a1, a2)((h, acc) => Cons(h, acc))
  }

  def appendLists[A](as: Lista[Lista[A]]): Lista[A] = {
    //foldRight(as, Lista[A]())((h, acc) => appendFoldRight(h, acc))
    foldRight(as, Lista[A]())(appendFoldRight)
  }

  // -------------------- Sesion 7 --------------------

  def addOne(l: Lista[Int]): Lista[Int] = {
    foldRight(l, Vacio: Lista[Int])((elem, acc) => Cons(elem + 1, acc))
  }

  def doubleToString(l: Lista[Double]): Lista[String] = {
    foldRight(l, Vacio: Lista[String])((elem, acc) => Cons(elem.toString, acc))
  }

  def map[A, B](l: Lista[A])(f: A => B): Lista[B] = {
    foldRight(l, Vacio: Lista[B])((elem, acc) => Cons(f(elem), acc))
  }

  def filter[A](l: Lista[A])(f: A => Boolean): Lista[A] = {
    foldRight(l, Vacio: Lista[A])((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)
  }

  def flatMap[A, B](l: Lista[A])(f: A => Lista[B]): Lista[B] = {
    appendLists(map(l)(f))
  }

  def filterFlatMap[A](l: Lista[A])(f: A => Boolean): Lista[A] = {
    flatMap(l)(x => if (f(x)) Lista(x) else Vacio)
  }

  def tieneSubsecuencia[A](lista: Lista[A], sub: Lista[A]): Boolean = ???

  def addLists(l1: Lista[Int], l2: Lista[Int]): Lista[Int] = {
    @tailrec
    def go(l1: Lista[Int], l2: Lista[Int], res: Lista[Int]): Lista[Int] = {
      (l1, l2) match {
        case (Vacio, Vacio) => res
        case (_, Vacio) => Vacio
        case (Vacio, _) => Vacio
        case (Cons(h, t), Cons(h2, t2)) => go(t, t2, append(res, Lista(h+h2)))
      }
    }

    go(l1, l2, Vacio)
  }

  def zipWith[A, B, C](l1: Lista[A], l2: Lista[B])(f: (A, B) => C): Lista[C] = {

    @tailrec
    def go(l1: Lista[A], l2: Lista[B], res: Lista[C]): Lista[C] ={
      (l1, l2) match {
        case (Vacio, Vacio) => res
        case (_, Vacio) => Vacio
        case (Vacio, _) => Vacio
        case (Cons(h, t), Cons(h2, t2)) => go(t, t2, append(res, Lista(f(h,h2))))
      }
    }

    go(l1, l2, Vacio)

  }

}



