

//0, 1, 1, 2, 3, 5
def fibX(n: Int): Int = {
  //    @tailrec
  def go(n: Int): Int = n match {
    case x if x < 0 => 0
    case 0 => 0
    case 1 => 1
    // fib n = fib (n-1 ) + fib ( n-2)
    case x => go(x - 1) + go(x - 2)
  }

  go(n)
}

def fib(n: Int): Int = {

  def go(x: Int, prev: Int, next: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else if (x == n) prev + next
    else go(x + 1, next, prev + next)
  }

  go(2, 0, 1)
}

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
/*
 0 = 0
 1 = 1
 2 = 1
 3 = 2
 4 = 3
 5 = 5



 */