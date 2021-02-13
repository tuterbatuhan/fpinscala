package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  //  foldRight using foldLeft
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil //Can also throw error
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("List is empty")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  // f is in the second parameter,so the the type information can be carried from the input l to the input f.
  // This helps us by not needing to give the type information for f when calling dropWhile3
  def dropWhile3[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => 1 + acc)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    //    foldRight(a1, a2)((h:A, z:List[A]) => Cons(h,z))
    foldRightViaFoldLeft(a1, a2)(Cons(_, _))
  }

  def sum3(ns: List[Int]) = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Int]) = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRightViaFoldLeft(l, Nil: List[A])(append2)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, head) => Cons(head, acc))
  }

  def addOne(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(l, Nil: List[A])(
      (h, t) => {
        if (f(h)) Cons(h, t)
        else t
      })
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    //    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => append2(f(h),t))
    concat(map(l)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addPairWise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) => {
      if (h1 == h2) hasSubsequence(t1, t2)
      else hasSubsequence(t1, b)
    }
  }

}

object TestDropWhile {

  import List._

  def main(args: Array[String]): Unit = {
    println(dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x <= 3))
    println(dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x <= 8))
    // Notice that we don't need to give the type information for x because scala now knows that x is type Int since the first parameter (l) is type Array[Int]
    println(dropWhile3(List(1, 2, 3, 4, 5, 6))(x => x <= 3))
  }
}

object TestInit {

  import List._

  def main(args: Array[String]): Unit = {
    println(init(List(1, 2, 3)))
    println(init(Nil))
  }
}

object TestFoldRight {

  import List._

  def main(args: Array[String]): Unit = {
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    println(foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  }
}

object TestLength {

  import List._

  def main(args: Array[String]): Unit = {
    println(length(List(1, 2, 3)))
    println(length(List(1, 2, 3, 4, 5)))
    println(length2(List(1, 2, 3, 4, 5)))
  }
}

object TestSum {

  import List._

  def main(args: Array[String]): Unit = {
    println(sum2(List(1, 2, 3)))
    println(sum3(List(1, 2, 3)))
  }
}

object TestProduct {

  import List._

  def main(args: Array[String]): Unit = {
    println(product2(List(2, 3, 4)))
    println(product3(List(2, 3, 4)))
  }
}

object TestReverse {

  import List._

  def main(args: Array[String]): Unit = {
    println(reverse(List(2, 3, 4)))
  }
}

object TestAppend2 {

  import List._

  def main(args: Array[String]): Unit = {
    println(append2(List(1, 2), List(3, 4)))
  }
}

object TestConcatLists {

  import List._

  def main(args: Array[String]): Unit = {
    println(concat(List(List(1, 2), List(3, 4), List(5))))
  }
}

object TestAddOne {

  import List._

  def main(args: Array[String]): Unit = {
    println(addOne(List(1, 2, 3)))
  }
}

object TestDoubleToString {

  import List._

  def main(args: Array[String]): Unit = {
    println(doubleToString(List(1.0, 2.0, 3.0)))
  }
}

object TestMap {

  import List._

  def main(args: Array[String]): Unit = {
    println(map(List(1, 2, 3))(_ + 4))
  }
}

object TestFilter {

  import List._

  def main(args: Array[String]): Unit = {
    println(filter(List(1, 2, 3, 4))(_ % 2 == 0))
  }
}

object TestFlatMap {

  import List._

  def main(args: Array[String]): Unit = {
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
  }
}

object TestAddPairWise {

  import List._

  def main(args: Array[String]): Unit = {
    println(addPairWise(List(1, 2, 3), List(4, 5, 6)))
    println(addPairWise(List(1, 2), List(4, 5, 6)))
    println(addPairWise(List(1, 2, 3), List(4, 5)))
  }
}

object TestZipWith {

  import List._

  def main(args: Array[String]): Unit = {
    println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))
    println(zipWith(List(1, 2), List(4, 5, 6))(_ + _))
    println(zipWith(List(1, 2, 3), List(4, 5))(_ + _))
  }
}

object TestHasSubsequence {

  import List._

  def main(args: Array[String]): Unit = {
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 4)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 5)))
    println(hasSubsequence(List(1, 2, 3, 4), List(5, 2)))
    println(hasSubsequence(List(1, 2, 3, 4), List(5)))
  }
}