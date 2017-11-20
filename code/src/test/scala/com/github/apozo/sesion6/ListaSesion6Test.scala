package com.github.apozo.sesion6

import com.github.apozo.sesion5.{Lista, Vacio}
import com.github.apozo.sesion6.Vacio
import com.github.apozo.sesion6.Lista._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by apozo.
  */
class ListaSesion6Test extends FlatSpec with Matchers with PropertyChecks {

  "sumFold" should "work the same as sum" in {
    sumFold(Lista()) should be(sum(Lista()))
    sumFold(Vacio) should be(sum(Vacio))
    sumFold(Lista(1, 2)) should be(sum(Lista(1, 2)))
    sumFold(Lista(-1, 2)) should be(sum(Lista(-1, 2)))
    sumFold(Lista(16)) should be(sum(Lista(16)))
    sumFold(Lista(16, -20)) should be(sum(Lista(16, -20)))
  }


  "productFold" should "work the same as product" in {
    productFold(Lista()) should be(product(Lista()))
    productFold(Vacio) should be(product(Vacio))
    productFold(Lista(1.5, 2)) should be(product(Lista(1.5, 2)))
    productFold(Lista(-1, 2)) should be(product(Lista(-1, 2)))
    productFold(Lista(16)) should be(product(Lista(16)))
    productFold(Lista(16, -20)) should be(product(Lista(16, -20)))
  }

  "length" should "be 0 for empty lists" in {
    val l = Lista()
    assert(Lista.length(l) == 0)
  }

  it should "be the actual length for bigger lists" in {
    assert(Lista.length(Lista(1, 2, 5, 1)) == 4)
    assert(Lista.length(Lista(6)) == 1)
  }

  "sumFoldLeft" should "work the same as sum" in {
    sumFoldLeft(Lista()) should be(sum(Lista()))
    sumFoldLeft(Vacio) should be(sum(Vacio))
    sumFoldLeft(Lista(1, 2)) should be(sum(Lista(1, 2)))
    sumFoldLeft(Lista(-1, 2)) should be(sum(Lista(-1, 2)))
    sumFoldLeft(Lista(16)) should be(sum(Lista(16)))
    sumFoldLeft(Lista(16, -20)) should be(sum(Lista(16, -20)))
  }


  "productFoldLeft" should "work the same as product" in {

    productFoldLeft(Lista()) should be(product(Lista()))
    productFoldLeft(Vacio) should be(product(Vacio))
    productFoldLeft(Lista(1.5, 2)) should be(product(Lista(1.5, 2)))
    productFoldLeft(Lista(-1, 2)) should be(product(Lista(-1, 2)))
    productFoldLeft(Lista(16)) should be(product(Lista(16)))
    productFoldLeft(Lista(16, -20)) should be(product(Lista(16, -20)))
  }


  "lengthFoldLeft" should "be the same as length" in {
    val l = Lista()
    assert(lengthFoldLeft(l) == Lista.length(l))
    assert(lengthFoldLeft(Lista(1, 2, 5, 1)) == Lista.length(Lista(1, 2, 5, 1)))
    assert(lengthFoldLeft(Lista(6)) == Lista.length(Lista(6)))
  }

  "reverse" should "be the list itself for empty lists or one-element lists" in {
    val l = Lista()
    val l1 = Lista(5)
    assert(reverse(l) == l)
    assert(reverse(l1) == l1)
  }

  it should "be the reversed list for bigger lists" in {
    val l = Lista(1, 2, 3, 4, 5)
    assert(reverse(l) == Lista(5, 4, 3, 2, 1))
  }


  "appendfoldRight" should "be the same as append" in {
    val l1: Lista[Int] = Lista()
    val l2: Lista[Int] = Lista(2, 3, 4)
    val l3: Lista[Int] = Vacio
    assert(appendFoldRight(l1, l2) == append(l1, l2))
    assert(appendFoldRight(l3, l2) == append(l3, l2))
    assert(appendFoldRight(l1, l3) == append(l1, l3))
    assert(appendFoldRight(l2, l1) == append(l2, l1))
    assert(appendFoldRight(l1, l3) == append(l1, l3))
    assert(appendFoldRight(l3, l1) == append(l3, l1))

    val l4: Lista[Int] = Lista(1)
    val l5: Lista[Int] = Lista(2, 3, 4)
    val l6: Lista[Int] = Lista(5, 6)
    assert(appendFoldRight(l5, l4) == append(l5, l4))
    assert(appendFoldRight(l4, l6) == append(l4, l6))
    assert(appendFoldRight(appendFoldRight(l4, l5), l6) == append(append(l4, l5), l6))
  }


  "productFoldRightLeft" should "work the same as product" in {

    productFoldRightLeft(Lista()) should be(product(Lista()))
    productFoldRightLeft(Vacio) should be(product(Vacio))
    productFoldRightLeft(Lista(1.5, 2)) should be(product(Lista(1.5, 2)))
    productFoldRightLeft(Lista(-1, 2)) should be(product(Lista(-1, 2)))
    productFoldRightLeft(Lista(16)) should be(product(Lista(16)))
    productFoldRightLeft(Lista(16, -20)) should be(product(Lista(16, -20)))
  }


  "productFoldLeftRight" should "work the same as product" in {

    productFoldLeftRight(Lista()) should be(product(Lista()))
    productFoldLeftRight(Vacio) should be(product(Vacio))
    productFoldLeftRight(Lista(1.5, 2)) should be(product(Lista(1.5, 2)))
    productFoldLeftRight(Lista(-1, 2)) should be(product(Lista(-1, 2)))
    productFoldLeftRight(Lista(16)) should be(product(Lista(16)))
    productFoldLeftRight(Lista(16, -20)) should be(product(Lista(16, -20)))
  }

  "sumFoldRightLeft" should "work the same as sum" in {

    sumFoldRightLeft(Lista()) should be(sum(Lista()))
    sumFoldRightLeft(Vacio) should be(sum(Vacio))
    sumFoldRightLeft(Lista(1, 2)) should be(sum(Lista(1, 2)))
    sumFoldRightLeft(Lista(-1, 2)) should be(sum(Lista(-1, 2)))
    sumFoldRightLeft(Lista(16)) should be(sum(Lista(16)))
    sumFoldRightLeft(Lista(16, -20)) should be(sum(Lista(16, -20)))
  }


  "sumFoldLeftRight" should "work the same as sum" in {

    sumFoldLeftRight(Lista()) should be(sum(Lista()))
    sumFoldLeftRight(Vacio) should be(sum(Vacio))
    sumFoldLeftRight(Lista(1, 2)) should be(sum(Lista(1, 2)))
    sumFoldLeftRight(Lista(-1, 2)) should be(sum(Lista(-1, 2)))
    sumFoldLeftRight(Lista(16)) should be(sum(Lista(16)))
    sumFoldLeftRight(Lista(16, -20)) should be(sum(Lista(16, -20)))
  }


  "lengthLeftRight" should "be the same as length" in {
    val l = Lista()
    assert(lengthLeftRight(l) == Lista.length(l))
    assert(lengthLeftRight(Lista(1, 2, 5, 1)) == Lista.length(Lista(1, 2, 5, 1)))
    assert(lengthLeftRight(Lista(6)) == Lista.length(Lista(6)))
  }


  "lengthRightLeft" should "be the same as length" in {
    val l = Lista()
    assert(lengthRightLeft(l) == Lista.length(l))
    assert(lengthRightLeft(Lista(1, 2, 5, 1)) == Lista.length(Lista(1, 2, 5, 1)))
    assert(lengthRightLeft(Lista(6)) == Lista.length(Lista(6)))
  }


  "appendLists" should "empty for empty lists" in {
    val l: Lista[Lista[Int]] = Lista()
    assert(appendLists(l) == Vacio)
  }

  it should "return a plain list" in {
    val l1 = Lista(Lista(1))
    val l2 = Lista(Lista(1, 2, 3), Lista(4), Lista(5, 6, 7))
    assert(appendLists(l1) == Lista(1))
    assert(appendLists(l2) == Lista(1, 2, 3, 4, 5, 6, 7))
  }

}


