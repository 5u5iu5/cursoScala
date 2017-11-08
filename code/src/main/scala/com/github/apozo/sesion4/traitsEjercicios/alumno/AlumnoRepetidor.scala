package com.github.apozo.sesion4.traitsEjercicios.alumno

case class AlumnoRepetidor(nombre:String, apellidos: String) extends AlumnoTrait {
  override def isRepetidor: Boolean = true
}
