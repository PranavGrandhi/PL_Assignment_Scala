// generic abstract class Tree, parameterized by a type T
abstract class Tree[+T]

// generic case class Node, also parameterized by type T, 
// extends Tree[T] and represents an interior node that has a label of type T, 
// a left subtree, and a right subtree (both of type Tree[T]).
case class Node[T](label: T, left: Tree[T], right: Tree[T]) extends Tree[T]

// A generic case class Leaf, parameterized by type T,
// extends Tree[T] and represents a leaf that has a label of type T.
case class Leaf[T](label: T) extends Tree[T]

// generic trait Addable, parameterized by a type T
trait Addable[T] 
{
  def +(other: T): T
}

// class A that implements the Addable trait
class A(val value: Int) extends Addable[A] 
{
  // The result of adding two A operands together is an A object constructed 
  // with the sum of the integers within the two operand objects.
  def +(other: A): A = new A(this.value + other.value)

  // toString() method of A is overridden to show the integer value 
  // stored within it as well as to indicate that the object is of type A.
  override def toString: String = s"A($value)"
}

// class B that extends A
class B(value: Int) extends A(value) 
{
  // overrides the toString() method to show the integer 
  // and to indicate that the object is a B.
  override def toString: String = s"B($value)"
}

// class C that extends B
class C(value: Int) extends B(value) 
{
  // overrides the toString() method to show the integer 
  // and to indicate that the object is a C.
  override def toString: String = s"C($value)"
}


object Part2 
{
  // A generic function, inOrder, that is parameterized by type T 
  // and computes the list of labels found in a tree, in in-order order.
  def inOrder[T](tree: Tree[T]): List[T] = tree match {
    case Leaf(value) => List(value)
    case Node(value, left, right) => inOrder(left) ::: List(value) ::: inOrder(right)
  }

  // A generic function treeSum, parameterized by type T such that any such T has to implement the Addable trait,
  // which computes the sum of all the labels in a tree.
  def treeSum[T <: Addable[T]](tree: Tree[T]): T = tree match {
    case Leaf(value) => value
    case Node(value, left, right) => value + treeSum(left) + treeSum(right)
  }

  // A generic function treeMap (analogous to MAP in Scheme or ML) which applies a function to every label in a tree, 
  // returning a tree of the results. treeMap, for any types T and V, 
  // should take a function of type T=>V and a tree of type Tree[T] as parameters and return a tree of type Tree[V] as a result.
  def treeMap[T, V](f: T => V, tree: Tree[T]): Tree[V] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Node(value, left, right) => Node(f(value), treeMap(f, left), treeMap(f, right))
  }

  // A function BTreeMap that takes a function of type B=>B and 
  // a tree of type Tree[B] and (just like for TreeMap, above) applies the function to every label in the tree, 
  // returning a tree of type Tree[B] as the result.
  def BTreeMap(f: B => B, tree: Tree[B]): Tree[B] = treeMap(f, tree)

  def test(): Unit = {
    def faa(a: A): A = new A(a.value + 10)
    def fab(a: A): B = new B(a.value + 20)
    def fba(b: B): A = new A(b.value + 30)
    def fbb(b: B): B = new B(b.value + 40)
    def fbc(b: B): C = new C(b.value + 50)
    def fcb(c: C): B = new B(c.value + 60)
    def fcc(c: C): C = new C(c.value + 70)
    def fac(a: A): C = new C(a.value + 80)
    def fca(c: C): A = new A(c.value + 90)

    val myBTree: Tree[B] = Node(new B(4), Node(new B(2), Leaf(new B(1)), Leaf(new B(3))), 
                                Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))

    val myATree: Tree[A] = myBTree

    println("inOrder = " + inOrder(myATree))
    println("Sum = " + treeSum(myATree))

    // These are the valid function calls for BTree
    println(BTreeMap(fab,myBTree))
    println(BTreeMap(fbb,myBTree))
    println(BTreeMap(fbc,myBTree))
    println(BTreeMap(fac,myBTree))
    
    // These are the valid function calls for treeMap
    println(treeMap(faa,myATree))
    println(treeMap(fab,myATree))
    println(treeMap(fac,myATree))

    // These commented out lines are invalid for the following reasons
    
    // These are the BTreeMap calls that are Invalid for the following type errors
    // println(BTreeMap(faa,myBTree))	// This is a type error: faa expects A, but BTreeMap provides B
    // println(BTreeMap(fba,myBTree)) // This is a type error: fba expects B and returns A, but BTreeMap expects B and returns B
    // println(BTreeMap(fcb,myBTree))	// This is a type error: fcb expects C, returns B, but BTreeMap provides B
    // println(BTreeMap(fcc,myBTree)) // This is a type error: fcc expects C, but BTreeMap provides B
    // println(BTreeMap(fca,myBTree)) // This is a type error: fca expects C, returns A, but BTreeMap provides B

    // These are the treeMap calls that are Invalid for the following type errors
    // println(treeMap(fba,myATree)) 	// This is a type error: fba expects B, but treeMap is working with Tree[A]
    // println(treeMap(fbb,myATree)) 	// This is a type error: fbb expects B, but treeMap is working with Tree[A]
    // println(treeMap(fbc,myATree)) 	// This is a type error: fbc expects B, but treeMap is working with Tree[A]
    // println(treeMap(fcb,myATree)) 	// This is a type error: fcb expects C, but treeMap is working with Tree[A]
    // println(treeMap(fcc,myATree)) 	// This is a type error: fcc expects C, but treeMap is working with Tree[A]
    // println(treeMap(fca,myATree)) 	// This is a type error: fca expects C, but treeMap is working with Tree[A]
  }

  def main(args: Array[String]): Unit = {
    // call test
    test()
  }
}
