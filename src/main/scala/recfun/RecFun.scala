package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def sqrt(x: Double) =
    def abs(value: Double) = if (value < 0) -value else value

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def square(v: Double) = v * v

    def isGoodEnough(guess: Double) = abs(square(guess) - x) < 0.001

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)

  def main(args: Array[String]): Unit =
    // println(sqrt(2))
    println("Pascal's Triangle")
    println(pascal(1, 1))
    // for row <- 0 to 10 do
    //   for col <- 0 to row do
    //     print(s"${pascal(col, row)} ")
    //   println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    // Solution based on the binomial coefficient
    // let calculate a factorial number tail recursively
    def abs(value: Int) = if (value < 0) -value else value

    extension (int: Int) {
      def ! = fac(int)
    }

    def fac(int: Int): Int =
      @tailrec def facTail(numbers: List[Int], acc: Int): Int =
        numbers match {
          case Nil    => acc
          case h :: t => facTail(t, acc * h)
        }
      int match {
        case 0 => 1
        case v => facTail((1 to abs(v)).toList, 1)
      }
    
    (r.!)/((c.!)*((r-c).!))

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = ???

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
