package com.github.apozo.sesion2

/**
  * Created by apozo.
  */
object Sesion2 {

  def sum(x: Int, y: Int): Int = {
    x + y
  }

  def addToList(list: List[Int], elem: Int): List[Int] = list :+ elem

  def isPalindrome(list: List[Int]): Boolean = list == list.reverse


  val romanos = Map(1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V", 6 -> "VI", 7 -> "VII", 8 -> "VIII", 9 -> "IX", 10 → "X")

  def printMap(myMap: Map[Int, String]) = {
    myMap.foreach {
      case (k, v) => println(s"${k} => ${v}")
    }
  }

  def printSortedMap(myMap: Map[Int, String]) = {
    myMap.toList.sortBy(_._1).foreach {
      case (k, v) => println(s"${k} => ${v}")
    }
  }

  def aplicaInteres(cant: Double, tipo: Option[Double]): Double = {
    cant * tipo.getOrElse(1.5)
  }

  def aplicaInteres2(cant: Option[Double], tipo: Option[Double]): Option[Double] = {
    (cant, tipo) match {
      case (Some(c), Some(t)) => Some(c * t)
      case _ => None
    }
  }

  def aplicaInteresEither(cant: Either[String, Double], tipo: Either[String, Double]): Either[String, Double] = {
    (cant, tipo) match {
      case (Left(e1), Left(e2)) => Left(s"errores encontrados: [${e1}, ${e2}]")
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(c), Right(t)) => Right(c * t)
    }
  }

  def penultimate(list: List[Int]): Option[Int] =
    if (list.size >= 2) list.reverse.drop(list.head).headOption else Option.apply(0)


  def duplicates(list: List[Int], k: Int): List[Int] = list.flatMap(e => List.fill(k)(e))

  def rotate(list: List[Int], x: Int): List[Int] = {
    val controlPosition = x % list.size
    controlPosition match {
      case 0 => list
      case d if (d > 0) => list.drop(d) ::: list.take(d)
      case e if (e < 0) => list.drop(list.size + e) ::: list.take(list.size + e)
    }
  }

  def isPalindrome(word: String): Boolean = word.reverse.mkString equals( word)


}








