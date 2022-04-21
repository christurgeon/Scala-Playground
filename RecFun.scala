package recfun

import scala.collection.immutable.LazyList.cons

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  def pascal(c: Int, r: Int): Int = 
    if (c+1 == 1 || r+1 == 1 || c == r) then 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  def balance(chars: List[Char]): Boolean = 
    def iter(chars: List[Char], running: Int): Boolean =
      if (chars.isEmpty) running == 0
      else 
        if (chars.head == '(') then iter(chars.tail, running + 1)
        else 
          if (chars.head == ')') then running > 0 && iter(chars.tail, running - 1)
          else iter(chars.tail, running)
    iter(chars, 0)

  def countChange(money: Int, coins: List[Int]): Int = 
      if (money == 0) then 1
      else 
        if (money > 0 && !coins.isEmpty) then 
          countChange(money, coins.tail) + // don't take it
          countChange(money - coins.head, coins) // take the first one
        else 0

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 1 else f(a) * product(f)(a + 1, b)

  def factorial(n: Int): Int = 
    product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, base: Int)(a: Int, b: Int): Int =
    def recur(a: Int): Int = 
      if a > b then base 
      else combine(f(a), recur(a + 1))
    recur(a)

  def sum(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)

  def product(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)