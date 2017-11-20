package com.github.apozo.sesion9

import scala.util.{Failure, Success, Try}

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

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    (this, b) match {
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (Left(error), _) => Left(error)
      case (_, Left(error)) => Left(error)
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either {


  def mean(xs: Seq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("Empty Seq")
    else Right(xs.sum / xs.length)
  }


  def calcularCuotaString(age: String, incidencias: String): Either[String, Double] = {


    def convertToEither(v: String): Either[String, Int] = {
      Try(v.toInt) match {
        case Failure(e) => Left(e.getMessage)
        case Success(v) => Right(v)
      }
    }
    //    convertToEither(age).map2(convertToEither(incidencias))((a,b) => calcularCuota(a,b))
    //    convertToEither(age).map2(convertToEither(incidencias))(calcularCuota)

    val eitherAge = convertToEither(age)
    val eitherInc = convertToEither(incidencias)

    eitherAge.map2(eitherInc)(calcularCuota)
  }

  def calcularCuota(age: Int, incidencias: Int): Double = {
    age * incidencias
  }


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a.foldRight(Right(Nil): Either[E, List[A]])((elem, acc) => elem.map2(acc)(_ :: _))
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldRight(Right(Nil): Either[E, List[B]])((elem, acc) => f(elem).map2(acc)(_ :: _))
  }

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(x => x)
  }
}
