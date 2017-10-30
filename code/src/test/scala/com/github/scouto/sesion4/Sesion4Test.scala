package com.github.scouto.sesion4

import com.github.scouto.sesion4.applyUnapply.{Administracion, Alumno, Asignatura}
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Gen


class Sesion4Test extends FlatSpec with Matchers{
  val manjarin = Alumno("Javier", "Manjarin")
  val claudio  = Alumno(_nombre = "Claudio", _apellidos = "Barragan")
  val alfredo   = Alumno(_apellidos = "Santaelena", _nombre = "Alfredo")
  val fran  = Alumno("Fran", "González")
  val genPositiveInteger = for (n <- Gen.choose(-500, 500)) yield n

  val asignatura = Asignatura(
    nombre = "curso scala",
    descripcion = "Curso impartido en Amaris"
    ,limiteAlumnos = 3)

  "Administracion" should "permitir inscribirse si hay plazas" in {
    val adm = Administracion(Map(asignatura -> List(manjarin, claudio)))
    val optAdm = adm.alta(alfredo, asignatura)

    optAdm.get.relacionAlumnos should be (Map(asignatura -> List(alfredo, manjarin, claudio)))
  }

  it should "rechazar la inscipcion si ya está inscrito" in {
    val adm = applyUnapply.Administracion(Map(asignatura -> List(manjarin, claudio)))

    adm.alta(manjarin, asignatura) should be (None)
  }

  it should "rechazar la inscipcion si no quedan plazas" in {
    val adm = applyUnapply.Administracion(Map(asignatura -> List(manjarin, claudio, alfredo)))

    adm.alta(fran, asignatura) should be (None)
  }

  it should "levantar un mensaje de error si el alumno no estaba inscrito" in {
    val adm = applyUnapply.Administracion(Map(asignatura -> List(manjarin, claudio, alfredo)))
    val result = adm.baja(fran, asignatura)

    result should be (Left("Alumno no inscrito"))
  }

  it should "permitir si el alumno esta presente" in {
    val adm = applyUnapply.Administracion(Map(asignatura -> List(manjarin, claudio, alfredo)))
    val result = adm.baja(manjarin, asignatura)

    result.right.get.relacionAlumnos(asignatura).size should be (2)
    result.right.get.relacionAlumnos should be (Map(asignatura -> List(claudio, alfredo)))
  }

  it should "permitir instancia la clase y pintar sus parametros" in {
    CaseClassExample("Alvaro", 36).toString shouldBe "Alvaro - 36"
  }

  "fib" should "be equals to the addition of the two previous fib or n if n < 2" in {

//    forAll(genPositiveInteger) { (n: Int) =>
//      whenever(n >= 0) {
//
//        val result = fib(n)
//        if (n <= 1) result shouldEqual n
//        else result shouldEqual fib(n - 1) + fib(n - 2)
//      }
//    }
  }
}
