package com.github.apozo.sesion9

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import com.github.apozo.sesion9.Either._

/**
  * Created by couto on 30/06/17.
  */
class EitherTest extends FlatSpec with Matchers with PropertyChecks {

  val invalid = Left("error")
  val validI = Right(5)
  val validIUnder0 = Right(-5)
  val validS = Right("hi")
  val functionI = (a: Int) => a * 5
  val functionIEither = (a: Int) => if (a > 0) Right(a * 5) else Left("error")
  val functionS = (s: String) => s.toUpperCase


  "map" should "be Left for Left" in {
    assert(invalid.map(functionI) == invalid)
    assert(invalid.map(functionS) == invalid)
  }

  it should "be a valid Right values" in {
    assert(validI.map(functionI) == Right(25))
    assert(validS.map(_.toUpperCase) == Right("HI"))
  }


  "flatMap" should "be left for left" in {
    assert(invalid.flatMap(functionIEither) == Left("error"))
  }

  it should "be a valid Some for Some values" in {

    assert(validI.flatMap(functionIEither) == Right(25))
    assert(validIUnder0.flatMap(functionIEither) == Left("error"))
  }


  "orElse" should "be the default value if None" in {
    assert(invalid.orElse(Right("Nothing")) == Right("Nothing"))
    assert(invalid.orElse(Left("anotherError")) == Left("anotherError"))
  }

  it should "be the value inside Some" in {
    assert(validI.orElse(Right(45)) == Right(5))
  }


  "mean" should "be None empty list" in {
    assert(mean(Seq()) == Left("Empty Seq"))
  }

  it should "be the mean value otherwise" in {
    assert(mean(Seq(5.0)) == Right(5))
    assert(mean(Seq(1,2,3)) == Right(2.0))
  }

  "map2" should "be Left if any is Left" in {
    assert((Left("error"): Either[String, Int]).map2[String, Int, Int](Right(5))((a, b) => a+b) == invalid)
    assert(Right(5).map2[String, Int, Int](Left("error"): Either[String, Int])((a, b) => a+b) == Left("error"))
  }

  it should "be the function result otherwise" in {
    assert(Right(25).map2(Right(5))((a, b) => a+b) == Right(30))
    assert(Right(25).map2(Right(5))((a, b) => a * b) == Right(125))
  }

  "sequence" should "be Left if the list contains at least one Left" in {
    assert(sequence(List(Right(5), Left("error"))) == Left("error"))
  }

  it should "be Some if the list does not contain any None value" in {
    assert(sequence(Nil) == Right(Nil))
    assert(sequence(List(Right(5), Right(7))) == Right(List(5,7)))
  }

  "traverse" should "be Left if the list contains at least one Left" in {
    assert(traverse(List(5, -1))(x => if (x > 0) Right(x) else Left("error")) == Left("error"))
  }

  it should "be Right if the list does not contain any None value" in {
    assert(traverse(Nil: List[Int])(x => if (x > 0) Right(x) else Left("error")) == Right(Nil))
    assert(traverse(List(5,7))(x => if (x > 0) Right(x) else Left("error")) == Right(List(5,7)))
  }



}
