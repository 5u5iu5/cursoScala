package com.github.apozo.sesion7

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import com.github.apozo.sesion7.Arbol._


class ArbolTest extends FlatSpec with Matchers with PropertyChecks {

  val leaf  = Hoja(7)
  val tree =  Rama(Hoja(8), Rama(Hoja(15), Hoja(-1)))
  val x2tree =  Rama(Hoja(16), Rama(Hoja(30), Hoja(-2)))
  val boolArbol =  Rama(Hoja(true), Rama(Hoja(true), Hoja(false)))

  "size" should "be 1 for Hoja" in {
    assert(Arbol.size(leaf) == 1)
  }

  it should "be the addition of branches and leaves" in {
    assert(Arbol.size(tree) == 5)
  }

  "maximum" should "be the actual value for a single Leaf" in {
    assert(maximum(leaf) == 7)
  }

  it should "be maximum value in tree" in {
    assert(maximum(tree) == 15)
  }

  "depth" should "be 1 for Leaf" in {
    assert(depth(leaf) == 1)
  }

  it should "be the actual depth for a tree" in {
    assert(depth(tree) == 3)
  }

  "map" should "work  properly" in {
    assert(map(leaf)(_*2) == Hoja(14))
    assert(map(tree)(_*2) == x2tree)
    assert(map(tree)(_>0) == boolArbol)
  }

  "sizefold" should "work the same as size" in {
    assert(sizeFold(leaf) == Arbol.size(leaf))
    assert(sizeFold(tree) == Arbol.size(leaf))

  }

  "maximumFold" should "work the same as maximum" in {
    assert(maximumFold(leaf) == maximum(leaf))
    assert(maximumFold(tree) == maximum(tree))
  }



  "depthFold" should "work the same as depth" in {
    assert(depthFold(leaf) == depth(leaf))
    assert(depthFold(tree) == depth(tree))

  }


  "mapFold" should "work the same as map" in {
    assert(mapFold(leaf)(_*2) == map(leaf)(_*2))
    assert(mapFold(tree)(_*2) == map(tree)(_*2))
    assert(mapFold(tree)(_>0) == map(tree)(_>0))
  }


}

