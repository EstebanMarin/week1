package recfun

import scala.annotation.tailrec
import javax.management.RuntimeErrorException
import scala.util.*

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
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()
    // println(getunit(stringTest))

  // val stringTest = "hackthegame"
  val stringTest = "falfal"

  def getunit(s: String): Int = {
    val utilData: Map[Char, List[Char]] =
      s.toList.groupBy(identity).filter(x => x._2.size == 1)

    def result =
      s.map(utilData.get).filter(x => x != None).head

    def masaje(s: String): Option[Int] =
      for
        r: List[Char] <- result
        fc = s.indexOf(r.head)
      yield fc

    if utilData.isEmpty then -1 else masaje(s).getOrElse(-1)
  }

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

    (r.!) / ((c.!) * ((r - c).!))

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
    def checkAdd(h: Char, acc: Int): Int =
      h match {
        case '(' => acc + 1
        case ')' => acc - 1
        case _   => acc
      }
    @tailrec def blT(cha: List[Char], acc: Int): Boolean =
      cha match {
        case Nil                            => acc == 0
        case h :: _ if checkAdd(h, acc) < 0 => false
        case h :: t                         => blT(t, checkAdd(h, acc))
      }
    blT(chars, 0)

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    var cache = List.empty
    val range = (0 to money).toList
    def computeValue(amount: Int, coin: Int, prevList: List[Int]) =
      def getValue(inIndex: Int, list: List[Int]): Option[Int] = Try(
        list(inIndex)
      ) match {
        case Success(v) => Some(v)
        case Failure(e) => None
      }

    @tailrec def genNewList(
        r: List[Int],
        coin: Int,
        acc: List[Int],
        prevList: List[Int]
    ): List[Int] =
      r match {
        case Nil    => acc
        case 0 :: t => genNewList(t, coin, acc :+ 1, prevList)
        case h :: t => genNewList(t, coin, acc, prevList)
      }

    def gNList(cache: List[Int], currentCoin: Int): List[Int] =
      @tailrec def rangeLoop(range: List[Int], cache: List[Int]): List[Int] = range match {
        case Nil => cache
        case 1 :: t => rangeLoop(t, cache :+ 1)
        case h :: t => rangeLoop(t, computeCoins(h, cache))
      }
      ???

    def last = 
      coins.sorted.foldLeft(cache)((cache: List[Int], currentCoin: Int) => gNList(cache,currentCoin))
    //return value
    coins.sorted.foldLeft(cache)(gNList).head
    
