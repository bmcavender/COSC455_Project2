def prime (i: Int) : Boolean = { Range(2, i - 1).filter(i % _ == 0).length == 0 }

prime(7)


def twinprimes (i : Int, j : Int) : Boolean = {
  if (prime(i) && prime(j)) {
    i match {
      case i => if (i == j - 2) return true
    }
  }
  return false
}

twinprimes(41,43)



def twinprimeslist(n : Int) : List[Int] = {
  tpl(2, n).distinct
}

twinprimeslist(99)

def tpl(i : Int, n : Int) : List[Int] = {
  (i >= n) match {
    case true => Nil
    case false =>
      twinprimes(i, i + 2) match {
        case false => tpl(n, i + 1)
        case true => i::i+2::tpl(n, i + 1)
      }
  }
}

def goldbach(n: Int) : Unit = {
  (n >= 2 && n % 2 == 0) match {
    case false => Nil
    case true =>
      gbach(n, n - 2) match {
        case Nil =>
        case head :: tail => println(head + " + " + tail.head + " = " + n + " ")
      }
  }
}

def gbach(i: Int, n: Int) : List[Int] = {
  (i >= n) match {
    case true => Nil
    case false =>
      (i <= n && prime(i)) match {
        case false => tpl(n, n - 1)
        case true => i::gbach(n - i, n - i)
      }
  }
}

goldbach(20)