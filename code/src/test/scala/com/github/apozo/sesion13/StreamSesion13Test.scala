package com.github.apozo.sesion13

import com.github.apozo.sesion13.{Empty, Stream}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by couto on 30/06/17.
  */
class StreamSesion13Test extends FlatSpec with Matchers with PropertyChecks {


  val genPositiveInteger = for (n <- Gen.choose(0, 50)) yield n

  val stream = Stream(1,2,3)
  val streamString = Stream("1","2","3")
  val size1Stream = Stream("Hello")
  val emptyStream = Stream()
  val emptyIntStream: Stream[Int] = Stream()







  "zip" should "do this" in {
    val s1 = Stream(1, 3, 6, 7, 10)
    val s2 = Stream(1, 2, 3, 4, 5)
    val s3 = Stream(1, 2, 3)

    s1.zip(s2).toList should be (List((1, 1), (3,2), (6,3), (7,4), (10,5)))
    s2.zip(s3).toList should be (List((1,1),(2,2),(3,3)))

  }


  "zipAll" should "do this" in {
    val s1 = Stream(1, 3, 6, 7, 10)
    val s2 = Stream(1, 2, 3, 4, 5)
    val s3 = Stream(2, 1, 3)

    s1.zipAll(s2).toList should be (List((Some(1),Some(1)), (Some(3),Some(2)), (Some(6),Some(3)), (Some(7),Some(4)), (Some(10),Some(5))))
    s2.zipAll(s3).toList should be (List((Some(1),Some(2)), (Some(2),Some(1)), (Some(3),Some(3)), (Some(4), None), (Some(5), None)))
  }



  "empiezaPor" should "do this" in {
    val l1 = Stream(1, 2,3, 4)
    assert(l1.empiezaPor(Stream(1)))
    assert(l1.empiezaPor(Stream(1, 2)))
    assert(!l1.empiezaPor(Stream(2)))
    assert(!l1.empiezaPor(Stream(2, 3)))
    assert(!l1.empiezaPor(Stream(4)))
    assert(l1.empiezaPor(Empty))
    assert(emptyIntStream.empiezaPor(Empty))
    //emptyIntStream.empiezaPor(Stream(1)) should be (false)
  }

  "tieneSubsecuencia" should "do this" in {
    val l1 = Stream(1, 2,3, 4)
    assert(l1.tieneSubsecuencia(Stream(1)))
    assert(l1.tieneSubsecuencia(Stream(1, 2)))
    assert(l1.tieneSubsecuencia(Stream(2)))
    assert(l1.tieneSubsecuencia(Stream(2, 3)))
    assert(l1.tieneSubsecuencia(Stream(4)))
    assert(!l1.tieneSubsecuencia(Stream(1, 3)))
    assert(l1.tieneSubsecuencia(Empty))
    assert(l1.tieneSubsecuencia(Stream(1,2)))

  }





}
