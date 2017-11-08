package com.github.apozo.sesion5


case class Gato(nombre: String) extends Animal
case class Perro(nombre: String) extends Animal
case class Vaca(nombre: String) extends Animal

object Covarianza extends App {

  def imprimirNombres(animales: List[Animal]): Unit = {
    animales.foreach { animal =>
      println(animal.nombre)
    }
  }

  val gatos = List(Gato("Felix"), Gato("Isidoro"))
  val perros = List(Perro("Luigi"), Perro("Nanaki"))
  val vacas = List(Vaca("Rosalinda"), Vaca("Aurora"))


  imprimirNombres(gatos)
  println()
  imprimirNombres(perros)
  println()
  imprimirNombres(vacas)

}
