package com.github.scouto.sesion4.traitsEjercicios

import com.github.scouto.sesion4.traitsEjercicios.alumno.{AlumnoNuevo, AlumnoTrait}
import com.github.scouto.sesion4.traitsEjercicios.asignatura.{AsignaturaConPrioridad, AsignaturaTrait}

case class AdministracionTrait[asignatura <: AsignaturaTrait, alumno <: AlumnoTrait]
(relacionAlumnos: Map[asignatura, List[alumno]] = Map()) {

  def removeFirstElement(list: List[alumno], f: AlumnoTrait => Boolean): List[alumno] = {

    @annotation.tailrec
    def go(acc: List[alumno], rest: List[alumno]): List[alumno] = {

      rest match {
        case Nil => acc
        case h :: t if f(h) => acc ::: t
        case h :: t if !f(h) => go(acc :+ h, t)
      }
    }

    go(List(), list)
  }

  def alta(alumno: alumno, asignatura: asignatura): Option[AdministracionTrait[asignatura, alumno]] = {

    def altaConPrioridad(alumno: alumno, asignatura: asignatura): Option[AdministracionTrait[asignatura, alumno]] = {

      relacionAlumnos.getOrElse(asignatura, List()) match {
        case Nil => Some(AdministracionTrait(relacionAlumnos + (asignatura -> List(alumno))))
        case l if l.contains(alumno) => None
        case l if l.size < asignatura.plazas
        => Some(AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: l))))
        case l if l.size == asignatura.plazas
          && l.exists(_.isRepetidor)
        => Some(AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: removeFirstElement(l, a => a.isRepetidor)))))

        case _ => None
      }
    }


    def altaSinPrioridad(alumno: alumno, asignatura: asignatura): Option[AdministracionTrait[asignatura, alumno]] = {
      relacionAlumnos.getOrElse(asignatura, List()) match {
        case Nil => Some(AdministracionTrait(relacionAlumnos + (asignatura -> List(alumno))))
        case l if l.contains(alumno) => None
        case l if l.size < asignatura.plazas => Some(AdministracionTrait(relacionAlumnos + (asignatura -> (alumno :: l))))
        case _ => None
      }
    }


    (asignatura, alumno) match {
      case (AsignaturaConPrioridad(_, _, _), AlumnoNuevo(_, _)) => altaConPrioridad(alumno, asignatura)
      case _ => altaSinPrioridad(alumno, asignatura)
    }

  }

  def baja(alumno: alumno, asignatura: asignatura): Either[String, AdministracionTrait[asignatura, alumno]] = {

    val alumnos = relacionAlumnos.getOrElse(asignatura, List())

    alumnos match {
      case Nil => Left("Alumno no inscrito")
      case l if l.contains(alumno) => Right(AdministracionTrait(relacionAlumnos + (asignatura -> alumnos.filterNot(_ == alumno))))
      case _ => Left("Alumno no inscrito")
    }

  }
}


