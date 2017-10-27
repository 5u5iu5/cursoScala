package com.github.scouto.sesion4

import scala.annotation.tailrec

class ManejoListaExcepciones {
  def removeFirstElement[A](list: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def checkElement[A](elem: A, listRemain: List[A]): List[A] = {
      list match {
        case Nil => listRemain
        case head :: tail if f(head) => head :: tail
        case head :: tail if !f(head) => checkElement(elem, tail)
      }
    }
    checkElement(0, list)
  }

}
