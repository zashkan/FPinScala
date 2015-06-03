package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	def size[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Branch(l,r) => size(l)+size(r)+1
	}

	def maximum[A](t: Tree[Int]): Int = t match {
		case Leaf(x) => x
		case Branch(l,r) => maximum(l) max maximum(r)
	}

	def depth[A](t: Tree[A]): Int = t match {
		case Leaf(x) => 0
		case Branch(l,r) => 1+ (depth(l) max depth(r))
	}

	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(x) => Leaf(f(x))
		case Branch(l,r) => Branch(map(l)(f), map(r)(f))
	}

	// def fold[A,B](t: Tree[A], z: B)(f: (B,B) => B): B = t match {
	// 	case Leaf(x) => z
	// 	case Branch(l,r) => f(fold(l, z)(f), fold(r, z)(f))
	// }

	def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
		case Leaf(x) => f(x)
		case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

	def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_+_+1)

	def maximum2[A](t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

	def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((x,y) => 1 + (x max y))

	def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch.apply)	

}

