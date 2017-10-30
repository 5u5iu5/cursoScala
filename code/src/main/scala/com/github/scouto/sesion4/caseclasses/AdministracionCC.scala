package com.github.scouto.sesion4.caseclasses

/**
  * Created by scouto.
  */
case class AdministracionCC(relacionAlumnos: Map[AsignaturaCC, List[AlumnoCC]] = Map()) {

  /**
    * Debe dar de alta un alumno si no supera el máximo y el alumno no está ya presente
    * @param alumno
    * @param asignatura
    * @return
    */
  def alta(alumno: AlumnoCC, asignatura: AsignaturaCC): Option[AdministracionCC] = {
    val alumnos = relacionAlumnos.getOrElse(asignatura, List())
    alumnos match {
      case Nil => Some(AdministracionCC(relacionAlumnos + (asignatura -> List(alumno))))
      case l if l.contains(alumno) => None
      case l if l.size < asignatura.plazas => Some(AdministracionCC(relacionAlumnos + (asignatura -> (alumno :: l))))
      case _ => None
    }
  }

  /**
    * Debe dar de baja un alumno o levantar un error si no es posible
    * @param alumno
    * @param asignatura
    * @return
    */
  def baja(alumno: AlumnoCC, asignatura: AsignaturaCC): Either[String, AdministracionCC] = {

    val alumnos = relacionAlumnos.getOrElse(asignatura, List())

    alumnos match {
      case Nil => Left("Alumno no inscrito")
      case l if l.contains(alumno) => Right(AdministracionCC(relacionAlumnos + (asignatura -> alumnos.filterNot(_ == alumno))))
      case _ => Left("Alumno no inscrito")
    }
  }
}


