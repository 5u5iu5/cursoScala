package com.github.apozo.sesion10

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by couto on 30/06/17.
  */
class StreamTest extends FlatSpec with Matchers with PropertyChecks {

    val stream = Stream(1,2,3)
    val size1Stream = Stream("Hello")
    val emptyStream = Stream()
    val emptyIntStream: Stream[Int] = Stream()

  "headOption" should "return None for empty Streams" in {
      emptyStream.headOption should be (None)
  }

  it should "return Some(headValue) for empty Streams" in {
    stream.headOption should be (Some(1))
  }

  "toList" should "return Empty List for empty Streams" in {
    emptyStream.toList should be (List())
    emptyStream.toList should be (Nil)
  }

  it should "return the list with the same values for non-empty Streams" in {
    stream.toList should be (List(1,2,3))
  }


  "drop" should "be empty for empty streams" in {
    emptyStream.drop(5) should be (Stream())
    emptyStream.drop(0) should be (Stream())
    emptyStream.drop(-5) should be (Stream())
  }

  it should "be empty for one-element streams" in {
    size1Stream.drop(5) should be (Stream())
  }

  it should "be the same stream for 0 or below 0 drops regardless the stream itself" in {
    emptyStream.drop(0) should be (emptyStream)
    stream.drop(0) should be (stream)
    size1Stream.drop(0) should be (size1Stream)

    emptyStream.drop(-5) should be (emptyStream)
    stream.drop(-1) should be (stream)
    size1Stream.drop(-2) should be (size1Stream)
  }

  it should "be the stream without the x elements at the beginning for bigger streams" in {
   stream.drop(2).toList should be (List(3))
   stream.drop(4) should be (Stream())
   stream.drop(1).toList should be (List(2,3))
   stream.drop(15) should be (Stream())

  }


  "dropWhile" should "be empty for empty streams" in {
    emptyIntStream.dropWhile(_ > 5) should be (Stream())
  }

  it should "remove elements as longer as the predicates satisfies" in {

    stream.dropWhile(_ < 3).toList should be (List(3))
    stream.dropWhile(_ %2 != 0).toList should be (List(2,3))
    stream.dropWhile(_ < 10) should be (Stream())
    stream.dropWhile(x => true) should be (Stream())
    stream.dropWhile(x => false).toList should be (List(1, 2, 3))

  }


  "take" should "be empty for empty streams" in {
    emptyStream.take(5) should be (Stream())
    emptyStream.take(0) should be (Stream())
    emptyStream.take(-5) should be (Stream())
  }

  it should "be empty when parameter is 0 or below 0" in {
    emptyStream.take(0) should be (emptyStream)
    stream.take(0) should be (emptyStream)
    size1Stream.take(0) should be (emptyStream)

    emptyStream.take(-5) should be (emptyStream)
    stream.take(-5) should be (emptyStream)
    size1Stream.take(-5) should be (emptyStream)
  }


  it should "be the stream with the x elements at the beginning for bigger streams" in {
    stream.take(2).toList should be (List(1,2))
    stream.take(4).toList should be (List(1,2,3))
    stream.take(1).toList should be (List(1))
  }

  "takeWhile" should "be empty for empty streams" in {
    emptyIntStream.takeWhile(_ > 5) should be (Stream())
  }

  it should "take elements as longer as the predicates satisfies" in {

    stream.takeWhile(_ < 3).toList should be (List(1,2))
    stream.takeWhile(_ %2 != 0).toList should be (List(1))
    stream.takeWhile(_ < 10).toList should be (List(1, 2, 3))
    stream.takeWhile(_ < -10) should be (Stream())
    stream.takeWhile(x => false) should be (Stream())
    stream.takeWhile(x => true).toList should be (List(1, 2, 3))

  }

}
