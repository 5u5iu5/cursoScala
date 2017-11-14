package com.github.apozo.sesion8

/**
  * Created by scouto.
  */
object Sesion8 extends App{

  val xs = List(1,2,3)
  val ys = List("a", "b", "c")

  val mapped = for {
    x <- xs
  } yield x+1

  println("mapped: " +mapped)

  val filtered = for  {
    x <- xs
    if x % 2 == 0
  } yield x

  println("filtered: " + filtered)


  val filteredMapped = for {
    x <- xs
    if x % 2 == 0
  } yield x + 1

  println("filteredMapped: " + filteredMapped)


  val flatMappedOriginal = xs.flatMap(x => ys.map(y => (x, y)))

  println("flatMappedOriginal: " + flatMappedOriginal)

  val flatMapped = for {
    x <- xs
    y <- ys
  } yield (x,y)

  println("flatMapped: " + flatMapped)

  val all = for {
    x <- xs if x % 2 == 0
    y <- ys
  } yield (x, y)

  println("all: " + all)



}
