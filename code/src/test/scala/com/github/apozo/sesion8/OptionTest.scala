package com.github.apozo.sesion8

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import com.github.apozo.sesion8.Option._

/**
  * Created by couto on 30/06/17.
  */
class OptionTest extends FlatSpec with Matchers with PropertyChecks {

  val validI = Some(5)
  val validIUnder0 = Some(-5)
  val validS = Some("hi")
  val functionI = (a: Int) => a * 5
  val functionIOption = (a: Int) => if (a > 0) Some(a * 5) else None
  val functionS = (s: String) => s.toUpperCase


  "map" should "be None for None" in {
    assert(None.map(functionI) == None)
    assert(None.map(functionS) == None)
  }

  it should "be a valid Some for Some values" in {
    assert(validI.map(functionI) == Some(25))
    assert(validS.map(_.toUpperCase) == Some("HI"))
  }

  "getOrElse" should "be the default value if None" in {
    assert(None.getOrElse("Nothing") == "Nothing")
  }

   it should "be the value inside Some" in {
    assert(validI.getOrElse(None) == 5)
  }

  "flatMap" should "be None for None" in {
    assert(None.flatMap(functionIOption) == None)
  }

  it should "be a valid Some for Some values" in {
    assert(validI.flatMap(functionIOption) == Some(25))
    assert(validIUnder0.flatMap(functionIOption) == None)
  }

  "orElse" should "be the default value if None" in {
    assert(None.orElse(Some("Nothing")) == Some("Nothing"))
    assert(None.orElse(None) == None)
  }

  it should "be the value inside Some" in {
    assert(validI.orElse(Some(45)) == Some(5))
  }

  "filter" should "be None if None" in {
    assert(None.filter((a:Int) => a > 0) == None)
  }

  it should "be the value inside Some" in {
    assert(validI.filter((a:Int) => a > 0) == Some(5))
    assert(validIUnder0.filter((a:Int) => a > 0) == None)
  }

  "mean" should "be None empty list" in {
    assert(mean(Seq()) == None)
  }

  it should "be the mean value otherwise" in {
    assert(mean(Seq(5.0)) == Some(5))
    assert(mean(Seq(1,2,3)) == Some(2.0))
  }

  "map2" should "be None if any is None" in {
    assert(map2[Int, Int, Int](None, Some(5))((a, b) => a+b) == None)
    assert(map2[Int, Int, Int](Some(5), None)((a, b) => a+b) == None)
  }

  it should "be the function result otherwise" in {
    assert(map2(Some(25), Some(5))((a, b) => a+b) == Some(30))
    assert(map2(Some(25), Some(5))((a, b) => a * b) == Some(125))
  }

  "sequence" should "be None if the list contains at least one None" in {
    assert(sequence(List(Some(5), None)) == None)
  }

  it should "be Some if the list does not contain any None value" in {
    assert(sequence(Nil) == Some(Nil))
    assert(sequence(List(Some(5), Some(7))) == Some(List(5,7)))
  }


  "traverse" should "be None if the list contains at least one None" in {
    assert(traverse(List(5, -1))(x => if (x > 0) Some(x) else None) == None)
  }

  it should "be Some if the list does not contain any None value" in {
    assert(traverse(Nil: List[Int])(x => if (x > 0) Some(x) else None) == Some(Nil))
    assert(traverse(List(5,7))(x => if (x > 0) Some(x) else None) == Some(List(5,7)))
  }

  "variance" should "be None for empty Seq" in {
    assert(variance(Seq()) == None)
  }

  it should "be 0.6 for 1, 2,3 list" in {
    val l = Seq(1.0,2.0,3.0)
    assert(variance(l).getOrElse(0.0) < 0.7 && variance(l).getOrElse(0.0) > 0.66)
  }


}
