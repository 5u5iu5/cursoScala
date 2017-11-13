package com.github.apozo.sesion8

/**
  * Created by couto
  */
sealed trait Option[+A] {

  def map[B] (f: A => B): Option[B] = ???

  def flatMap[B](f: A => Option[B]) : Option[B] = ???

  def getOrElse[B >: A] (default: => B): B = ???

  //devuelve el valor del Option si existe, en caso contrario devuelve el parámetro recibido
  def orElse[B >: A] (ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



object Option {


  def mean(xs:Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def calcularCuota(age: Int, incidencias: Int): Double = ???

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = ???

  def variance(xs: Seq[Double]): Option[Double] = ???

}