package s99

import Solutions._

trait ListsSolutions {

  // avoid scala List API

  def last[T](list: List[T]): T = list match {
    case head :: Nil => head
    case head :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }

  def penultimate[T](list: List[T]): T = list match {
    case head :: _ :: Nil => head
    case _ :: tail => penultimate(tail)
    case Nil => throw new NoSuchElementException
  }

  def nth[T](n: Int, list: List[T]): T = {
    if (n == 0) list.head
    else nth(n - 1, list.tail)
  }

  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case head :: tail => length(tail) + 1
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case head :: Nil => List(head)
    case head :: tail => reverse(tail) :+ head
    case Nil => throw new IllegalArgumentException
  }

  def isPalindrome[T](list: List[T]): Boolean = list match {
    case Nil => true
    case _ => (list == reverse(list))
    }

  def flatten(list: List[Any]): List[Any] = {
    def flattenAux(elem: Any): List[Any] = elem match {
      case Nil => Nil
      case head :: tail => flattenAux(head) ::: flattenAux(tail)
      case head => List(head)
    }
    flattenAux(list)
  }

  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: head2 :: tail if head == head2 => compress(head2 :: tail)
    case head :: tail => head :: compress(tail)
  }

  def pack[T](list: List[T]): List[List[T]] = ???
  def encode[T](list: List[T]): List[(Int, T)] = ???
  def encodeModified[T](list: List[T]): List[Any] = ???
  def decode[T](list: List[(Int, T)]): List[T] = ???
  def encodeDirect[T](list: List[T]): List[(Int, T)] = ???
  def duplicate[T](list: List[T]): List[T] = ???
  def duplicateN[T](n: Int, list: List[T]): List[T] = ???
  def drop[T](n: Int, list: List[T]): List[T] = ???
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = ???
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = ???
  def rotate[T](n: Int, list: List[T]): List[T] = ???
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???
  def range[T](i: Int, j: Int): List[Int] = ???
  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???
}
