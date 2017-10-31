package com.github.scouto.sesion5.covarianza

object VendingMachine extends App {
  val colasVM: VendingMachine[Cola] = new VendingMachine (List (new Cola, new Cola ))
  val softDrinksVM: VendingMachine[SoftDrinks] = colasVM.addAll (List (new Tonic) )
}

class VendingMachine[+A](val currentItem: Option[A], items: List[A]) {
  def this(items: List[A]) = this(None, items)

  def dispenseNext(): VendingMachine[A] =
    items match {
      case Nil => {
        if (currentItem.isDefined)
          new VendingMachine(None, Nil)
        else
          this
      }
      case h :: t => {
        new VendingMachine(Some(h), t)
      }
    }

  def addAll[B >: A](newItems: List[B]): VendingMachine[B] =
    new VendingMachine(items ++ newItems)
}

class Drinks

abstract class SoftDrinks extends Drinks {
  def name: String
}

abstract class Juice extends Drinks {
  def name: String
}

case class Cola(name: String = "Cola") extends SoftDrinks

case class Tonic(name: String = "Tonic") extends SoftDrinks

case class Orange(name: String = "Orange") extends Juice

case class Watermelon(name: String = "Watermelon") extends Juice



