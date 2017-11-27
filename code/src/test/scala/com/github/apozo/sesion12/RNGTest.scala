package com.github.apozo.sesion12

import com.github.apozo.sesion12.RNG.SimpleRNG
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scouto.
  */
class RNGTest extends FlatSpec with Matchers with PropertyChecks {
  val genPositiveInteger = for (n <- Gen.choose(0, 5000)) yield n
  val simpleRNG = SimpleRNG(42)
  val rng = RNG



  "nonNegativeInt" should "return always a positive number" in {
    var state: RNG = simpleRNG

    forAll(genPositiveInteger) { (n: Int) =>

      val (result, newState) = rng.nonNegativeInt(state)
      state = newState
      result should be >= 0
    }
  }

  "double" should "return always a positive number below 1" in {
    var state: RNG = simpleRNG

    forAll(genPositiveInteger) { (n: Int) =>

      val (result, newState) = rng.double(state)
      state = newState
      result should be >= 0.0
      result should be < 1.0
    }
  }

  "ints" should "match the given size" in {
    val (l, r) = rng.ints(5)(simpleRNG)
    l.length should be (5)
  }


  "unit" should "return the value with the very same rng" in {
    val (a, r) = rng.unit(5)(simpleRNG)
    a should be (5)
  }

  "map" should "return the value mapped and a different rng" in {
    val (a, r) = rng.map(rng.unit(5))(x => x+1)(simpleRNG)
    a should be (6)
  }

  "nonNegativeEven" should "return always a positive number" in {
    var state: RNG = simpleRNG

    forAll(genPositiveInteger) { (n: Int) =>

      val (result, newState) = rng.nonNegativeEven(state)
      state = newState
      result should be >= 0
      result % 2 should be (0)
    }
  }

  "doubleMap" should "work the same as double" in {
    var state: RNG = simpleRNG

    forAll(genPositiveInteger) { (n: Int) =>

      val (result, newState) = rng.doubleMap(state)
      state = newState
      result should be >= 0.0
      result should be < 1.0
    }
  }

  "map2" should "return the values mapped and a different rng" in {
    val (output1, r1) = rng.map2(rng.unit(5), rng.unit(7.9))((a, b) => if (a%2==0) a+b else a-b )(simpleRNG)
    val (output2, r2) = rng.map2(rng.unit(2), rng.unit(7.9))((a, b) => if (a%2==0) a+b else a-b )(simpleRNG)
    assert(output1 === -2.9 +- 0.0001)
    assert(output2 === 9.9 +- 0.0001)
  }


  "both" should "return the value mapped and a different rng" in {
    val (output, r) = rng.map2(rng.unit(5), rng.unit(7.9))((a, b) => (a+1, b+1))(simpleRNG)
    assert(output._1 === 6 )
    assert(output._2 === 8.9 +- 0.0001)
  }







}
