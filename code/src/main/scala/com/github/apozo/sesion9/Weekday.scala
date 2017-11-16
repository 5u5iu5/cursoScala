package com.github.apozo.sesion9

/**
  * Created by scouto.
  */
object Weekday extends Enumeration {

  val Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo = Value

  val Monday = Value("Lunes")
  val Tuesday = Value("Martes")
  val Wednesday = Value("Miercoles")
  val Thursday = Value("Jueves")
  val Friday = Value("Viernes")
  val Saturday = Value("Sabado")
  val Sunday = Value("Domingo")

}

object MyApp extends App{

  def laborable(weekday: Weekday.Value): Boolean = ???


}




