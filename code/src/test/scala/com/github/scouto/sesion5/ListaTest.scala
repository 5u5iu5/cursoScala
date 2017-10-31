package com.github.scouto.sesion5

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import com.github.scouto.sesion5.Lista._
/**
  * Created by scouto.
  */
class ListaTest extends FlatSpec with Matchers with PropertyChecks{



  "sum" should "return 0 for empty listas" in {
    sum(Lista()) should be (0)
    sum(Vacio) should be (0)
  }

  "sum" should "return the addition" in {
    sum(Lista(1,2)) should be (3)
    sum(Lista(-1,2)) should be (1)
    sum(Lista(16)) should be (16)
    sum(Lista(16, -20)) should be (-4)
  }

  "init" should "return a list minos the last element" in {
    init(Lista(1,2,3,4)) should be (Lista(1,2,3))
  }

  "product" should "return the proper value" in {
    product(Lista(1,2)) should be (2)
    product(Lista()) should be (1)
    product(Lista(-1,2)) should be (-2)
    product(Lista(16)) should be (16)
    product(Lista(5, 5)) should be (25)
  }

  }


