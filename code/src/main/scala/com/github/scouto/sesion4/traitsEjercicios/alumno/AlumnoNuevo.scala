package com.github.scouto.sesion4.traitsEjercicios.alumno

case class AlumnoNuevo(nombre:String, apellidos: String) extends AlumnoTrait {
  override def isRepetidor: Boolean = false

}
