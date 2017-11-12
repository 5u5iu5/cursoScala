package com.github.apozo.sesion8

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

/**
  * Created by scouto.
  */
class Sesion8Test extends FlatSpec with Matchers with PropertyChecks{

  "map" should "" in {

    val xs = List(1,2,3)

    val resultado1 = xs.map(_ +1)
    val resultado2 = for  {
      x <- xs
    } yield x+1

    assert(resultado1 == resultado2)
  }

  "filter" should "" in {

    val xs = List(1,2,3)

    val resultado1 = xs.filter(_ % 2 == 0)
    val resultado2 = for  {
      x <- xs
      if x % 2 == 0
    } yield x

    assert(resultado1 == resultado2)
  }

  "filteredMapped" should "" in {

    val xs = List(1,2,3)

    val resultado1 = xs.filter(_ % 2 == 0).map(_ +1)
    val resultado2 = for  {
      x <- xs
      if x % 2 == 0
    } yield x +1

    assert(resultado1 == resultado2)
  }


  "flatMapped" should "" in {

    val xs = List(1,2,3)
    val ys = List("a", "b", "c")

    val resultado1 = xs.flatMap(x => ys.map(y => (x, y)))
    val resultado2 = for  {
      x <- xs
      y <- ys
    } yield (x,y)

    assert(resultado1 == resultado2)
  }

  "allTogether" should "" in {

    val xs = List(1,2,3)
    val ys = List("a", "b", "c")

    val resultado1 = xs.filter(x => x % 2 == 0).flatMap(x => ys.map( y =>  (x, y)))


    val resultado2 = for {
      x <- xs if x % 2 == 0
      y <- ys
    } yield (x, y)

    assert(resultado1 == resultado2)
  }



}
