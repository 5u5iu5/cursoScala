package com.github.scouto

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import com.github.scouto.Sesion2._

/**
  * Created by scouto.
  */

class Sesion2Test extends FlatSpec with Matchers with PropertyChecks{

  val genPositiveInteger = for (n <- Gen.choose(-500, 500)) yield n
  val list = List.range(1,6)


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
    resultList.foreach(println)
    resultList.size shouldEqual (10)
    resultList(2) shouldEqual (2)
    resultList(3) shouldEqual (2)
  }

  "rotate" should "return a list with elem rotate n positions" in {
    //1,2,3,4,5 should be
    val result = rotate(list, 2)
    result(0) shouldEqual(3)
    result(1) shouldEqual(4)
    result(2) shouldEqual(5)

  }

  "palindrome" should "return true or false if word is palindrome" in {
    //Ababa, Abalaba, aibofobia, Ana, ala, arenera, arepera, anilina, ananá, aviva ,Malayalam, Menem, Neuquén, oro, Oruro, oso, ojo, radar, reconocer, rotor, salas, seres, somos, sometemos.
    isPalindrome("ababa") shouldBe (true)
    isPalindrome("arenera") shouldBe(true)
    isPalindrome("Alvaro") shouldBe(false)


  }
}
