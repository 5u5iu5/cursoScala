package com.github.apozo.sesion9

/**
  * Created by couto on 30/06/17.
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(algo) => Left(algo)
      case Right(algo) => Right(f(algo))
    }
  }

  def flatMap[EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(algo) => Left(algo)
      case Right(algo) => f(algo)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(algo) => Right(algo)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {a <- this; b1 <- b } yield f(a, b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either {


  def mean(xs: Seq[Double]): Either[String, Double] = ???

  def calcularCuota(age: Int, incidencias: Int): Double = {
    age * incidencias
  }


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = ???

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???
}
