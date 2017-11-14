package com.github.apozo.sesion8

object EjerciciosPag12 extends App {

  var stringList = List("Alvaro", "Laura", "Arya", "Jon")

  //With map
  var mapStringList = stringList.map(_.length.toInt)
  println(s"String list count with mapper $mapStringList")

  //With for comprehension
  var forStringList = for {
    elem <- stringList if elem.length.toInt % 2 == 0
  }yield (elem, elem.length.toInt)
  println(s"String list count with for comprehension $forStringList")

  var listaDeListas = List(List("Alvaro","Arya","Laura"), List("Laura","Jon"), List("Dani","Marina","Javi"), List("Nadie"))

  var resultListaDeLista = for {
    lista <- listaDeListas if lista.size >= 2
    elem <- lista if elem.length.toInt % 2 == 0
  }yield  (elem, elem.length.toInt)
  println(s"list into list $resultListaDeLista")

}
