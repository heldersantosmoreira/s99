package s99

import Solutions._

trait LogicAndCodesSolutions { outer =>

  implicit class ExtendedBoolean(a: Boolean) {
    def and(b: => Boolean): Boolean = outer.and(a, b)
    def or(b: => Boolean): Boolean = outer.or(a, b)
    def nand(b: => Boolean): Boolean = outer.nand(a, b)
    def nor(b: => Boolean): Boolean = outer.nor(a, b)
    def xor(b: => Boolean): Boolean = outer.xor(a, b)
    def impl(b: => Boolean): Boolean = outer.impl(a, b)
    def equ(b: => Boolean): Boolean = outer.equ(a, b)
  }

  def and(a: Boolean, b: => Boolean): Boolean = if (a) b else false
  def or(a: Boolean, b: => Boolean): Boolean = if (a) true else b
  def nand(a: Boolean, b: => Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: => Boolean): Boolean = not(or(a, b))
  def xor(a: Boolean, b: => Boolean): Boolean = a != b
  def impl(a: Boolean, b: => Boolean): Boolean = if (a) b else true
  def equ(a: Boolean, b: => Boolean): Boolean = not(xor(a, b))
  def not(a: Boolean): Boolean = if (a) false else true

  def table2(f: (Boolean, Boolean) => Boolean): String = {
    def options = List((true, true), (true, false), (false, true), (false, false))
    def toString(b: Boolean) = if (b) "true  " else "false "
    ("A     B     result" :: options.map {
      case (a, b) => List(a, b, f(a, b)).map(toString).mkString("")
    }).mkString("\n")
  }

  def gray(n: Int): List[String] = {
    if (n == 1) List("0", "1")
    else {
      val previous = gray(n-1)
      previous.map("0" + _) ++ previous.reverse.map("1" + _)
    }
  }

  def huffman(list: List[(String,  Int)]): List[(String, String)] = ???
}
