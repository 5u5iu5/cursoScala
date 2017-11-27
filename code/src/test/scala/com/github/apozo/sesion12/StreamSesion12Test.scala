package com.github.apozo.sesion12

import com.github.apozo.sesion12.Stream._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by couto on 30/06/17.
  */
class StreamSesion12Test extends FlatSpec with Matchers with PropertyChecks {


  val genPositiveInteger = for (n <- Gen.choose(0, 50)) yield n

  val stream = Stream(1,2,3)
  val streamString = Stream("1","2","3")
  val size1Stream = Stream("Hello")
  val emptyStream = Stream()
  val emptyIntStream: Stream[Int] = Stream()

  "mapUnfold" should "work the same as map" in {
    emptyIntStream.mapUnfold(_ * 5) should be ( emptyIntStream.map(_ * 5))
    stream.mapUnfold(_ * 5).toList should be (stream.map(_ * 5).toList)
    streamString.mapUnfold(_.toInt).toList should be (streamString.map(_.toInt).toList)
    size1Stream.mapUnfold(_.length).toList should be (size1Stream.map(_.length).toList)
  }


  "takeUnfold" should "work the same as take" in {
    emptyStream.takeUnfold(5) should be (emptyStream.take(5))
    emptyStream.takeUnfold(0) should be (emptyStream.take(0))
    stream.takeUnfold(0) should be (stream.take(0))
    size1Stream.takeUnfold(0) should be (size1Stream.take(0))
    stream.takeUnfold(-5) should be (stream.take(-5))
    size1Stream.takeUnfold(-5) should be (size1Stream.take(-5))
    stream.takeUnfold(2).toList should be (stream.take(2).toList)
    stream.takeUnfold(4).toList should be (stream.take(4).toList)
    stream.takeUnfold(1).toList should be (stream.take(1).toList)
  }

  "takeWhileUnfold" should "work as takeWhile" in {
    emptyIntStream.takeWhileUnfold(_ > 5) should be (emptyIntStream.takeWhile(_ > 5))
    stream.takeWhileUnfold(_ < 3).toList should be (stream.takeWhile(_ < 3).toList)
    stream.takeWhileUnfold(_ %2 != 0).toList should be (stream.takeWhile(_ %2 != 0).toList)
    stream.takeWhileUnfold(_ < 10).toList should be (stream.takeWhile(_ < 10).toList)
    stream.takeWhileUnfold(_ < -10) should be (stream.takeWhile(_ < -10))
    stream.takeWhileUnfold(x => true).toList should be (stream.takeWhile(x => true).toList)
    stream.takeWhileUnfold(x => false) should be (stream.takeWhile(x => false))
  }



  "zipWith" should "do this" in {
    val s1 = Stream(1, 3, 6, 7, 10)
    val s2 = Stream(1, 2, 3, 4, 5)
    val s3 = Stream(1, 2, 3)

    s1.zipWith(s2)((a, b) => a+b).toList should be (List(2, 5, 9, 11, 15))
    s1.zipWith(s2)((a, b) => a - b).toList should be (List(0, 1, 3, 3, 5))
    s2.zipWith(s3)((a, b) => a - b).toList should be (List(0,0,0))

  }


  "zipWithAll" should "do this" in {
    val s1 = Stream(1, 3, 6, 7, 10)
    val s2 = Stream(1, 2, 3, 4, 5)
    val s3 = Stream(2, 1, 3)

    s1.zipWithAll(s2)((a, b) => a.getOrElse(0)+b.getOrElse(0)).toList should be (List(2, 5, 9, 11, 15))
    s1.zipWithAll(s2)((a, b) => a.getOrElse(0)-b.getOrElse(0)).toList should be (List(0, 1, 3, 3, 5))
    s2.zipWithAll(s3)((a, b) =>  a.getOrElse(0)-b.getOrElse(0)).toList should be (List(-1,1,0, 4, 5))

  }

  "tails" should "be empty for empty streams" in {
    emptyStream.tails should be (Stream())
  }

  it should "be the stream itself plus empty for one-element streams" in {
      size1Stream.tails.map(_.toList).toList should be (List(List("Hello")))
  }

  it should "be the tails plus empty for bigger streams" in {
    stream.tails.map(_.toList).toList should be (List(List(1,2,3), List(2,3), List(3)))
  }









}
