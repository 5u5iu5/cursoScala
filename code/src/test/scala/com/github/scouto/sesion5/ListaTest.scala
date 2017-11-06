package com.github.scouto.sesion5

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import com.github.scouto.sesion5.Lista._

class ListaTest extends FlatSpec with Matchers with PropertyChecks{



  "sum" should "return 0 for empty listas" in {
    sum(Lista()) should be (0)
    sum(Vacio) should be (0)
  }

  it should "return the addition" in {
    sum(Lista(1,2)) should be (3)
    sum(Lista(-1,2)) should be (1)
    sum(Lista(16)) should be (16)
    sum(Lista(16, -20)) should be (-4)
  }

  "product" should "return 1.0 for empty listas" in {
    product(Lista()) should be (1.0)
    product(Vacio) should be (1.0)
  }

  it should "return the proper value" in {
    product(Lista(1,2)) should be (2)
    product(Lista()) should be (1)
    product(Lista(-1,2)) should be (-2)
    product(Lista(16)) should be (16)
    product(Lista(5, 5)) should be (25)
  }

  "tail" should "be empty for empty lists" in {
    val l = Lista()
    assert(tail(l) == Vacio)
  }

  it should "be empty for one-element lists" in {
    val l = Lista(1)
    assert(tail(l) == Vacio)
  }

  it should "be the tail itself for bigger lists" in {

    val lInt = Lista(1, 2, 5, 7)
    assert(tail(lInt) == Lista(2, 5, 7))

    val lString = Lista("myName", "mySurname")
    assert(tail(lString) == Lista("mySurname"))

    val lBool = Lista(true, true, false)
    assert(tail(lBool) == Lista(true, false))

  }

  "setHead" should "be one element list for empty lists" in {
    val l = Lista()
    setHead(l, 5) should be (Lista(5))
  }


  "setHead" should "be one element list for one-element lists" in {
    val l = Lista(7)
    setHead(l, 5) should be (Lista(5))
  }

  "setHead" should "be the same list with a new head" in {
    val l = Lista(7, 9, 10, 12)
    setHead(l, 5) should be (Lista(5, 9, 10, 12))
  }


  "drop" should "be empty for empty lists" in {
    val l = Lista()
    assert(drop(l, 5) == Vacio)
    assert(drop(l, 0) == Vacio)
    assert(drop(l, -5) == Vacio)
  }

  it should "be empty for one-element lists" in {
    val l = Lista(1)
    assert(drop(l, 5) == Vacio)
  }

  it should "be the same list for 0 drops regardless the list itself" in {
    val l = Lista(1)
    val ls = Lista("myName", "mySurname")
    assert(drop(l, 0) == l)
    assert(drop(Vacio, 0) == Vacio)
    assert(drop(ls, 0) == ls)
  }

  it should "be the list without the x elements at the beginning for bigger lists" in {

    val lInt = Lista(1, 2, 5, 7)
    assert(drop(lInt, 3) === Lista(7))
    assert(drop(lInt, 4) == Vacio)
    assert(drop(lInt, 1) == tail(lInt))
    assert(drop(lInt, 15) == Vacio)

  }

//  "dropWhile" should "be empty for empty lists" in {
//    val l: Lista[Int] = Lista()
//    assert(dropWhile(l)(x => x > 5) == Vacio)
//    assert(dropWhile(l)(_ > 5) == Vacio)
//    assert(dropWhile(l)(_ > 5) == Vacio)
//  }

  it should "remove elements as longer as the predicates satisfies" in {

    val lInt = Lista(1, 2, 5, 7)
    assert(dropWhile(lInt)(_ < 5) == Lista(5, 7))
    assert(dropWhile(lInt)(_ < 10) == Vacio)
    assert(dropWhile(lInt)(x => true) == Vacio)
    assert(dropWhile(lInt)(x => false) == Lista(1, 2, 5, 7))

  }

  "append" should "be the second list for empty lists" in {
    val l1: Lista[Int] = Lista()
    val l2: Lista[Int] = Lista(2,3,4)
    val l3: Lista[Int] = Vacio
    val l4: Lista[Int] = Lista(1,5,6)
    assert(append(l1, l2) == l2)
    assert(append(l3, l2) == l2)
    assert(append(l1, l3) == l3)
    assert(append(l2, l4) == Lista(2,3,4,1,5,6))
  }

  it should "be the first list for empty second lists" in {
    val l1: Lista[Int] = Lista()
    val l2: Lista[Int] = Lista(2,3,4)
    val l3: Lista[Int] = Vacio
    assert(append(l2, l1) == l2)
    assert(append(l1, l3) == l1)
    assert(append(l3, l1) == l3)
  }

  it should "be the appended list in any other case" in {
    val l1: Lista[Int] = Lista(1)
    val l2: Lista[Int] = Lista(2,3,4)
    val l3: Lista[Int] = Lista(5, 6)
    assert(append(l2, l1) == Lista(2,3,4, 1))
    assert(append(l1, l3) ==  Lista(1,5, 6))
    assert(append(append(l1, l2), l3) ==  Lista(1, 2,3,4,5, 6))
  }



  "init" should "be empty for empty lists" in {
    val l1: Lista[Int] = Lista()
    assert(init(l1) == Vacio)
  }

  it should "be the same list wo the last element" in {
    assert(init(Lista(5, 4)) == Lista(5))
    assert(init(Lista(5, 4, 3, 2, 1)) == Lista(5, 4, 3, 2))
  }



}


