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

  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    def encodeDirectAux(list: List[T], aux: List[(Int, T)]): List[(Int, T)] = (list, aux) match {
      case (Nil, Nil) => Nil
      case (Nil, aux) => aux
      case (head :: tail, Nil) => encodeDirectAux(tail, List((1, head)))
      case (h1 :: t1, h2 :: t2) if h1 == h2._2 => encodeDirectAux(t1, (h2._1 + 1, h2._2) :: t2)
      case (h1 :: t1, aux) => aux ::: encodeDirectAux(h1 :: t1, Nil)
    }
    encodeDirectAux(list, Nil)
  }

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

  def rotate[T](n: Int, list: List[T]): List[T] = {
    if (list.isEmpty) list
    else if (n > 0) rotate(n-1, list.tail ++ List(list.head))
    else if (n < 0) rotate(n+1, list.last :: (list.dropRight(1)))
    else list
  }

  // def removeAt[T](i: Int, list: List[T]): (List[T], T) = {
  //   def removeAtAux(i: Int, list: List[T], out: List[T]): (List[T], T) = {
  //     if (i > 0) removeAtAux(i-1, list.tail, out ++ List(list.head))
  //     else (out ++ list.tail, list.head)
  //   }

  //   removeAtAux(i, list, List())
  // }

  // another solution with no aux method
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = {
    if (i > 0) {
      val (result, elem) = removeAt(i-1, list.tail)
      (list.head :: result, elem)
    }
    else (list.tail, list.head)
  }

  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = {
    if (i > 0) list.head :: insertAt(t, i-1, list.tail)
    else t :: list
  }

  def range[T](i: Int, j: Int): List[Int] = {
    if (i > j) Nil
    else i :: range(i+1, j)
  }

  import scala.util.Random

  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    if (n > 0) {
      val (rem, elem) = removeAt(Random.nextInt(list.length), list)
      elem :: randomSelect(n-1, rem)
    } else Nil
  }

  def lotto[T](i: Int, j: Int): List[Int] = {
    randomSelect(i, range(1, j))
  }

  def randomPermute[T](list: List[T]): List[T] = randomSelect(list.length, list)

  // cool stuff!
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    if (n > 0) list match {
      case Nil => Nil
      case head :: tail => combinations(n, tail) ++ (for (combination <- combinations(n-1, tail)) yield head :: combination)
    }
    else List(Nil)
  }

  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???

  def lsort[T](list: List[List[T]]): List[List[T]] = {

    // a kind of bubble sort
    // def bubbleSort(source: List[List[T]], result: List[List[T]]) = {
    //   if (source.isEmpty) result
    //   else bubble(source, Nil, result)
    // }

    // def bubble(source: List[List[T]], tempList: List[List[T]], result: List[List[T]]): List[List[T]] = source match {
    //   case h1 :: h2 :: t =>
    //     if (h1.length > h2.length) bubble(h1 :: t, h2 :: tempList, result)
    //     else bubble(h2 :: t, h1 :: tempList, result)
    //   case h1 :: t => bubbleSort(tempList, h1 :: result)
    // }

    // bubbleSort(list, Nil)

    // using quicksort
    // picks head as pivot instead of middle element, so could be improved?
    def quicksort(list: List[List[T]]): List[List[T]] = {
      list match {
        case Nil => List()
        case head :: tail =>
          quicksort(for(x <- tail if x.length < head.length) yield x) :::
          List(head) :::
          quicksort(for(x <- tail if x.length >= head.length) yield x)
      }
    }

    quicksort(list)
  }

  // just too much work to avoid the List API
  // maybe in the future if I feel like it
  // TODO
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    list.groupBy(_.length).values.toList.sortBy(_.length).flatten
  }
}
