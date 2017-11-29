package com.github.apozo.sesion13

import com.github.apozo.sesion12.RNG
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

  "mapFlatMap" should "work the same way as map" in {
    val (a, r) = rng.map(rng.unit(5))(x => x+1)(simpleRNG)
    val (af, rf) = rng.mapFlatMap(rng.unit(5))(x => x+1)(simpleRNG)
    a should be (af)
  }

  "map2FlatMap" should "work the same way as map2" in {
    val (output1, r1) = rng.map2(rng.unit(5), rng.unit(7.9))((a, b) => if (a%2==0) a+b else a-b )(simpleRNG)
    val (output1f, r1f) = rng.map2FlatMap(rng.unit(5), rng.unit(7.9))((a, b) => if (a%2==0) a+b else a-b )(simpleRNG)
    val (output2, r2) = rng.map2(rng.unit(2), rng.unit(7.9))((a, b) => if (a%2==0) a+b else a-b )(simpleRNG)
    val (output2f, r2f) = rng.map2FlatMap(rng.unit(2), rng.unit(7.9))((a, b) => if (a%2==0) a+b else a-b )(simpleRNG)
    assert(output1 === output1f +- 0.0001)
    assert(output2 === output2f +- 0.0001)
  }








}
