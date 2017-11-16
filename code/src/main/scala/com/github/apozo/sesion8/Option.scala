package com.github.apozo.sesion8

/**
  * Created by couto
  */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(algo) => Some(f(algo))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(algo) => algo
    }
  }

  //devuelve el valor del Option si existe, en caso contrario devuelve el parámetro recibido
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse (ob)
    /*this match {
      case None => ob
      case  _ => this
    }*/
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(criteria => if (f(criteria)) Some(criteria) else None)
    /*this match {
      case Some(a) if f(a) => this
      case _ => None
    }*/
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def calcularCuota(age: Int, incidencias: Int): Double = ???

  // Lifting a la brava
  def calcularCuotaString(age: String, incidencias: String): Option[Double] = {
    val myAge = try {
      age.toInt
    } catch {
      case e: Exception => return None
    }

    val myIncidencia = try {
      incidencias.toInt
    } catch {
      case e: Exception => return None
    }

    Some(calcularCuota(myAge, myIncidencia))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aprima => b.map((bprima => f(aprima, bprima))))

    /*(a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a,b))
    }*/

    /*for {
      aprima <- a
      bprima <- b
    } yield f(aprima, bprima)*/
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight(Some(Nil):Option[List[A]])((e, acc) => map2(e, acc)((e, acc) => e::acc))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((e, acc) => map2(f(e), acc)((e, acc) => e::acc))


  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}