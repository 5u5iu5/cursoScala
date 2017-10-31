package com.github.scouto.sesion5

import scala.annotation.tailrec

/**
  * Created by scouto.
  */
object Sesion5 extends App{
//
//  Implicit Example
//    import java.time.LocalDate
//    val date1: ChronoLocalDate = LocalDate.parse("2017-02-07")
//    val date2: ChronoLocalDate = LocalDate.parse("2017-02-07")
//
//    import scala.math.Ordering.Implicits._
//
//    date1 >= date2




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

}
