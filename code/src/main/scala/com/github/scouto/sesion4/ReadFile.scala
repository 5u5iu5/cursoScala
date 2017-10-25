package com.github.scouto.sesion4

import scala.io.Source

/**
  * Created by scouto.
  */
object ReadFile extends App {
  val myFile = "loremipsum.txt"
  val buffer = Source.fromResource(myFile)

  try {
    for(line <- buffer.getLines) {
      println(line.toUpperCase)
    }
  } catch {
    case ex: Exception => println(s"Exception: ${ex.toString}")
  } finally {
    buffer.close
  }
}
