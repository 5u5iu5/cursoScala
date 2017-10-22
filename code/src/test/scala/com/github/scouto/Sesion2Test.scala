package com.github.scouto

/**
  * Created by scouto.
  */

class Sesion2Test extends FlatSpec with Matchers with PropertyChecks{

  val genPositiveInteger = for (n <- Gen.choose(-500, 500)) yield n



  "sum" should "work with both natural numbers" in {
    sum(2,3) should be (5)
  }

  "sum" should "work for all numbers" in {

    forAll(genPositiveInteger, genPositiveInteger) { (n1: Int, n2: Int) =>
        val result = sum(n1, n2)
        println(s"${n1} + ${n2} equals: ${result}")
        result shouldEqual n1 + n2
    }
  }

  "penultimate" should "return penultimate elem from list" in {

    penultimate(list) shouldEqual (Option(4))
  }

  "duplicate" should "duplicate n times to elements list" in {

    val resultList = duplicates(list, 2)
    resultList.size shouldEqual (12)
    resultList(1) shouldEqual (2)
    resultList(2) shouldEqual (2)
  }
}
