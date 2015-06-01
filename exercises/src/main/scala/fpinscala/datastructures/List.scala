package fpinscala.datastructures
//import scala.collection

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      //case Cons(0.0, xs) => 0.0
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    //case Nil => sys.error("tail of Nil list")
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs) 
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n<=0) l
    else l match {
      case Nil => Nil
      case Cons(x,xs) => drop(xs,n-1)
    }
  }

  // def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //   case Nil => Nil
  //   case Cons(x,xs) =>
  //     if (f(x))
  //       dropWhile(xs, f)
  //     else
  //       l
  // }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x,y) => y + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go[A,B](l: List[A], z: B, acc: B)(f: (B, A) => B): B = l match {
      case Nil => acc
      //case Cons(0, as) => 0
      case Cons(a, as) => go(as, z, f(acc, a))(f)
    }

    go(l, z, z)(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((x,y) => x + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go[A](l: List[A], buff: List[A]): List[A] = l match {
      case Nil => buff
      case Cons(a, as) => go(as, Cons(a, buff))
    }

    go(l, Nil)
  }

  def reverse2[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, a)=>Cons(a, acc))
    //foldLeft(l, Nil)((acc, a)=>Cons(a, acc))  
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_ , _))

  def concatenate[A](ls: List[A]*): List[A] = {
    if (ls.isEmpty) Nil
    else List.append2(ls.head, concatenate(ls.tail: _*))
  }

  // def add1[A](l: List[A]): List[A] = l match {
  //   case Nil => Nil
  //   case Cons(x,xs) => foldRight()
  // }

  def add1[A](l: List[Int]): List[Int] = foldRight(l, List[Int]())((x, y) => Cons(x + 1, y))
 
  def listDblToStr[A](l: List[Double]): String = foldLeft(l, "")((acc, a) => acc + "*" + a.toString)

  def listDblToListStr[A](l: List[Double]): List[String] = foldRight(l, List[String]())((x, xs) => Cons(x.toString, xs))
  
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((h,t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((h,t) => {if (f(h)) Cons(h, t) else t})

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((h, t) => append2(f(h), t))

  //def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = concatenate(map(l)(f))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) Cons(x, Nil) else List[A]())

  def addL(a: List[Int], b: List[Int]): List[Int] = a match {
    case Nil => Nil
    case Cons(x, xs) => b match {
      case Nil => Nil
      case Cons(y, ys) => Cons(x+y, addL(xs, ys))
    } 
  }

  def zipWith[A,B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }

  // @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x,xs), Cons(y,ys)) => ((x==y) && hasSubsequence(xs,ys)) || hasSubsequence(xs,sub)
  }

  @annotation.tailrec
  def startsWith[A](a: List[A], b: List[A]): Boolean = (a,b) match {
    case (_,Nil) => true
    case (Cons(x,xs), Cons(y,ys)) if (x==y) => startsWith(xs,ys)
    case(_,_) => false 
  }

   @annotation.tailrec
   def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
     case (_,Nil) => true
     case (Nil,_) => false
     case (_,_) if startsWith(sup,sub) => true
     case (Cons(x,xs), _) => hasSubsequence2(xs,sub) 
   }

}

/*
object myModule {
  def main(args: Array[String]): Unit ={
    val a = List(1,2,3)
    val b = List(9,8,7)
    println(a)
    println(List.sum(a))
    println(List.tail(a))
    println(List.tail(Nil))
    println(List.setHead(a,-9))
    println(List.setHead(Nil,-9))

    println(List.drop(Nil,5))
    println(List.drop(a,2))
    def f(x:Int): Boolean = x==3

    println(List.dropWhile(a,(t:Int)=>t==1))

    println("init of %s is %s".format(a, List.init(a)))

    println("dropWhile2 of %s is %s".format(a, List.dropWhile2(a)(_ == 1)))    
    println("length of %s is %s".format(a, List.length(a)))    

    println(List.sum3(a))
    println(List.product3(a))
    println(List.length2(a))

    println(List.reverse(a))

    println(List.append(a, b))
    println(List.append2(a, b))

    println(List.concatenate(a,b,a,b))
    println(List.add1(a))

    val c = List(1.5,6.3,8.8)
    println(List.listDblToStr(c))
    println(List.listDblToListStr(c))
    println(List.map(c)(_.toString))

    println(List.filter(a)(_>1))

    val d: List[Int] = List(1,2,3,5,7,9,10)
    println(List.filter(d)(_%2==0))
    println(List.filter2(d)(_%2==0))

    println(List.flatMap(a)(i => List(i,i*2, i*3)))
    println(List.addL(a,b))
    println(List.zipWith(a,b)(_*_))

    var cols: Map[Char,Int] = Map()
    cols += ('I' -> 5)
    println(cols)

    val l: List[Int] = List(1,3,2,2,6,5)
    println("sub: " + List.hasSubsequence(l, List(2,2,5)))
    println("sub2: " + List.hasSubsequence2(l, List(2,2))) 
    println("start: " + List.startsWith(List(1,2,3,4), List()))

    val t = Branch(Branch(Leaf(1),Leaf(2)),Branch(Branch(Leaf(5),Leaf(90)),Leaf(40)))
    println("tree size="+Tree.size(t))
    println("tree size2="+Tree.size2(t))   

    println("tree max="+Tree.maximum(t)((x,y) => x max y)) 
    println("tree max2="+Tree.maximum2(t)) 
    
    println("tree depth="+Tree.depth(t))   
    println("tree depth2="+Tree.depth2(t))  

    println("tree ="+t) 
    println("tree mapped="+Tree.map(t)(_-100))   
    println("tree mapped2="+Tree.map2(t)(_-100))   
  }
}
*/

