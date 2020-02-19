package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  sealed trait ImplementationFlag {
    private[Tree] def size[A](tree: Tree[A]): Int
    private[Tree] def maximum(tree: Tree[Int]): Int
    private[Tree] def depth[A](tree: Tree[A]): Int
    private[Tree] def map[A,B](tree: Tree[A])(f: A => B): Tree[B]
  }

  object WithoutFold extends ImplementationFlag {
    override private[Tree] def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

    override private[Tree] def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }

    override private[Tree] def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(left, right) => (depth(left) max depth(right)) + 1
    }

    override private[Tree] def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  }

  object WithFold extends ImplementationFlag {
    override private[Tree] def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

    override private[Tree] def maximum(tree: Tree[Int]): Int = fold(tree)(n => n)(_ max _)

    override private[Tree] def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0) { case (n, m) => (n max m) + 1}

    override private[Tree] def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))
  }

  def size[A](tree: Tree[A], useFold: ImplementationFlag = WithFold): Int = useFold.size(tree)
  def maximum(tree: Tree[Int], useFold: ImplementationFlag = WithFold): Int = useFold.maximum(tree)
  def depth[A](tree: Tree[A], useFold: ImplementationFlag = WithFold): Int = useFold.depth(tree)
  def map[A, B](tree: Tree[A], useFold: ImplementationFlag = WithFold)(f: A => B) : Tree[B] = useFold.map(tree)(f)
}
