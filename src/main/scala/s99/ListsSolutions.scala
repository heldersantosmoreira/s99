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

  def pack[T](list: List[T]): List[List[T]] = {
    def packAux(list: List[T], aux: List[T]): List[List[T]] = (list, aux) match {
      case (Nil, Nil) => Nil
      case (Nil, list) => List(list)
      case (head :: tail, Nil) => packAux(tail, List(head))
      case (h1 :: t1, list) if h1 == list.head => packAux(t1, h1 :: list)
      case (h1 :: t1, list) => list :: packAux(h1 :: t1, Nil)
    }
    packAux(list, Nil)
  }

  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list).map { elem => (elem.size, elem.head) }
  }

  def encodeModified[T](list: List[T]): List[Any] = encode(list) map {
    case (1, elem) => elem
    case tuple => tuple
  }

  // full List API
  // def decode[T](list: List[(Int, T)]): List[T] = {
  //   list.flatMap(el => List.fill(el._1)(el._2))
  // }

  // full recursion
  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case (1, el) :: tail => el :: decode(tail)
    case (n, el) :: tail => el :: decode((n-1, el) :: tail)
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = ???

  def duplicate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => head :: head :: duplicate(tail)
  }

  // full List API
  // def duplicateN[T](n: Int, list: List[T]): List[T] = {
  //   list.flatMap(el => List.fill(n)(el))
  // }

  // full recursion
  def duplicateN[T](n: Int, list: List[T]): List[T] = {
    def duplicateNAux[T](n: Int, el: T, out: List[T]): List[T] = n match {
      case 0 => out
      case _ => duplicateNAux(n-1, el, el :: out)
    }

    list match {
      case Nil => Nil
      case head :: tail => duplicateNAux(n, head, List()) ++ duplicateN(n, tail)
    }
  }

  def drop[T](n: Int, list: List[T]): List[T] = {
    def dropAux(k: Int, list: List[T], out: List[T]): List[T] = list match {
      case Nil => out
      case head :: tail if k == 1 => dropAux(n, tail, out)
      case head :: tail => dropAux(k-1, tail, out ++ List(head))
    }

    dropAux(n, list, List())
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {
    def splitAux(k: Int, in: List[T], out: List[T]): (List[T], List[T]) = k match {
      case 0 => (out, in)
      case k => splitAux(k-1, in.tail, out ++ List(in.head))
    }

    splitAux(n, list, List())
  }

  def slice[T](i: Int, j: Int, list: List[T]): List[T] = {
    if (i > 0) slice(i-1, j-1, list.tail)
    else if (j > 0) list.head :: slice(0, j-1, list.tail)
    else Nil
  }
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
