def composicionA(f: Int => String, g: Int => Int): Int => String = {
  a => f(g(a))
}

def composicionB(f: Int => String): Int => String = {
  a => f(a)
}

composicionB(3 => "3")

def toString(num: Int) = num.toString
