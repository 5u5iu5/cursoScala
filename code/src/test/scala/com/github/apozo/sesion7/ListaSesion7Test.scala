package com.github.apozo.sesion7

import com.github.apozo.sesion7.{Lista, Vacio}
import com.github.apozo.sesion7.Lista._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scouto.
  */
class ListaSesion7Test extends FlatSpec with Matchers with PropertyChecks{



  "addOne" should "be empty for empty lists" in {
    assert(addOne(Lista()) == Vacio)
    assert(addOne(Vacio) == Vacio)
  }

  it should "be a list containing all the elements otherwise" in {
    assert(addOne(Lista(1, 2, 3)) == Lista(2, 3, 4))
  }

  "doubleToString" should "be empty for empty lists" in {
    assert(doubleToString(Lista()) == Vacio)
    assert(doubleToString(Vacio) == Vacio)
  }

  it should "be a list containing all the elements otherwise" in {
    assert(doubleToString(Lista(1, 2, 3.5)) == Lista("1.0", "2.0", "3.5"))
  }

  "map" should "be empty for empty lists" in {
    assert(map(Lista[Int]())(a => a + 1) == Vacio)
    assert(map(Vacio: Lista[Int])(_ + 1) == Vacio)
  }

  it should "be the same as addOne" in {
    val l = Lista(1, 2, 3)
    assert(map(l)(_ + 1) == addOne(l))
  }

  it should "be the same as doubleToString" in {
    val l = Lista(1.1, 2, 3)
    assert(map(l)(_.toString) == doubleToString(l))
  }

  "filter" should "be empty for empty lists" in {
    assert(filter(Lista[Int]())(_ > 1) == Vacio)
    assert(filter(Vacio: Lista[Int])(_ > 1) == Vacio)
  }

  it should "filter all elements that not satisfy the predicate" in {
    val l = Lista(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val res = filter(l)(_ % 2 == 0)
    assert(res == Lista(2, 4, 6, 8, 10))
  }

  "flatMap" should "be empty for empty lists" in {
    val l:Lista[Int] = Vacio
    assert(flatMap(l)(a => Lista(a, a)) == Vacio)
  }

  it should "be the list <<aplanada>>" in {
    val l = Lista(1, 2, 3)
    assert(flatMap(l)(a => Lista(a, a)) == Lista(1, 1, 2, 2, 3, 3))
  }

  "filterFlatMap" should "work the same as filter" in {
    val l = Lista(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(filterFlatMap(Lista[Int]())(_ > 1) == filter(Lista[Int]())(_ > 1))
    assert(filterFlatMap(Vacio: Lista[Int])(_ > 1) == filter(Vacio: Lista[Int])(_ > 1))
    assert(filterFlatMap(l)(_ % 2 == 0) == filter(l)(_ % 2 == 0))
  }

  "addLists" should "do this" in {
    val l1 = Lista(1, 3, 6, 7, 10)
    val l2 = Lista(1, 2, 3, 4, 5)
    assert(addLists(l1, l2) == Lista(2, 5, 9, 11, 15))
  }

  "zipWith" should "do this" in {
    val l1 = Lista(1, 3, 6, 7, 10)
    val l2 = Lista(1, 2, 3, 4, 5)
    assert(addLists(l1, l2) == zipWith(l1, l2)((a, b) => a+b))
    assert(Lista(0, 1, 3, 3, 5) == zipWith(l1, l2)((a, b) => a - b))
  }

  }


