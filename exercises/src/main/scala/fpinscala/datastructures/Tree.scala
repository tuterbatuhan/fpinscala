package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => (depth(l) max depth(r)) + 1
    case Leaf(_) => 0
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A, B](t: Tree[A])(f1: A => B)(f2: (B, B) => B): B = t match {
    case Branch(l, r) => f2(fold(l)(f1)(f2), fold(r)(f1)(f2))
    case Leaf(v) => f1(v)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((x, y) => (x max y) + 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

}

object tests {

  import Tree._

  val myTree1 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  val myTree2 = Branch(Branch(Leaf(2), Leaf(1)), Branch(Leaf(8), Leaf(3)))
  val myTree3 = Branch(Branch(Leaf(2), Leaf(1)), Branch(Leaf(8), Branch(Leaf(5), Leaf(9))))

  def main(args: Array[String]): Unit = {
    //    testSize()
    //    testSizeViaFold()
    //    testMax()
    //    testMaxViaFold()
    //    testDepth()
    //    testDepthViaFold()
    testMap()
    testMapViaFold()
  }


  def testSize(): Unit = {
    println(size(myTree1))
    println(size(myTree2))
    println(size(myTree3))
  }

  def testSizeViaFold(): Unit = {
    println(sizeViaFold(myTree1))
    println(sizeViaFold(myTree2))
    println(sizeViaFold(myTree3))
  }

  def testMax(): Unit = println(maximum(myTree2))

  def testMaxViaFold(): Unit = println(maximumViaFold(myTree2))

  def testDepth(): Unit = println(depth(myTree3))

  def testDepthViaFold(): Unit = println(depthViaFold(myTree3))

  def testMap(): Unit = println(map(myTree1)((x: String) => x.toUpperCase))

  def testMapViaFold(): Unit = println(mapViaFold(myTree1)((x: String) => x.toUpperCase))
}