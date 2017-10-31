package com.github.scouto.sesion5

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import com.github.scouto.sesion5.Sesion5._
import org.scalacheck.Gen
/**
  * Created by scouto.
  */
class Sesion5Test extends FlatSpec with Matchers with PropertyChecks{

  val genPositiveInteger = for (n <- Gen.choose(-500, 500)) yield n
  val genInteger = for (n <- Gen.choose(-500, 500)) yield n
  val genIntList = Gen.containerOf[List, Int](genInteger)
  val genStringList = Gen.containerOf[List, String](Gen.alphaStr)
  val genBoolList = Gen.containerOf[List, Boolean](Gen.oneOf(true, false))

  implicit def intOrdering(a: Int, b: Int) : Boolean = a <= b
  //implicit def anotherIntOrdering(a: Int, b: Int) : Boolean = a >= b
  implicit def stringOrdering(a: String, b: String) : Boolean = a <= b
  def booleanOrdering(a: Boolean, b: Boolean) : Boolean = a <= b

  "isSorted" should "return true for empty arrays" in {
    val aInt: List[Int] = List()
    val aString: List[String] = List()
    val aBoolean: List[Boolean] = List()

    isSorted(aInt) shouldEqual true
    isSorted(aString) shouldEqual true
    isSorted(aBoolean)(booleanOrdering) shouldEqual true

  }

  it should "return true for one-element arrays" in {
    val aInt = List(1)
    val aString = List("myName")
    val aBoolean = List(true)

    isSorted(aInt) shouldEqual true
    isSorted(aString) shouldEqual true
    isSorted(aBoolean)(booleanOrdering) shouldEqual true

  }

  it should "return false for unsorted arrays and true for the same sorted arrays" in {

    val aInt = List(2, 1)
    val aString = List("yourName", "myName")
    val aBoolean = List(true, false)

    isSorted(aInt) shouldEqual false
    isSorted(aString) shouldEqual false
    isSorted(aBoolean)(booleanOrdering) shouldEqual false

    isSorted(aInt.sorted) shouldEqual true
    isSorted(aString.sorted) shouldEqual true
    isSorted(aBoolean.sorted)(booleanOrdering) shouldEqual true

  }

  it should "return true for any sorted arrays" in {

    forAll(genIntList) { (myList) =>
      isSorted(myList.sorted) shouldEqual true

    }

    forAll(genStringList) { (myList) =>
      isSorted(myList.sorted) shouldEqual true

    }

    forAll(genBoolList) { (myList) =>
      isSorted(myList.sorted)(booleanOrdering) shouldEqual true
    }


  }

}
