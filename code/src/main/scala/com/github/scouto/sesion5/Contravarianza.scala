package com.github.scouto.sesion5

/**
  * Created by scouto.
  */
abstract class Printer[-A] {
  def print(value: A): Unit
}

class AnimalPrinter extends Printer[Animal] {
  def print(animal: Animal): Unit =
    println("The animal's name is: " + animal.nombre)
}

class CatPrinter extends Printer[Gato] {
  def print(gato: Gato): Unit =
    println("The cat's name is: " + gato.nombre)
}


object Contravarianza extends App {
  val gato: Gato = Gato("Felix")

  def printMyCat(printer: Printer[Gato]): Unit = {
    printer.print(gato)
  }

  val catPrinter: Printer[Gato] = new CatPrinter
  val animalPrinter: Printer[Animal] = new AnimalPrinter

  printMyCat(catPrinter)
  printMyCat(animalPrinter)
}
