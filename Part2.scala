abstract class Tree[+T]

case class Node[T](label: T, left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[T](label: T) extends Tree[T]

trait Addable[T] {
  def +(other: T): T
}

class A(val value: Int) extends Addable[A] {
  def +(other: A): A = new A(this.value + other.value)

  override def toString: String = s"A($value)"
}

class B(value: Int) extends A(value) {
  override def toString: String = s"B($value)"
}

class C(value: Int) extends B(value) {
  override def toString: String = s"C($value)"
}


object Part2 {
  // inOrder function
  def inOrder[T](tree: Tree[T]): List[T] = tree match {
    case Leaf(value) => List(value)
    case Node(value, left, right) => inOrder(left) ::: List(value) ::: inOrder(right)
  }

  // treeSum function
  def treeSum[T <: Addable[T]](tree: Tree[T]): T = tree match {
    case Leaf(value) => value
    case Node(value, left, right) => value + treeSum(left) + treeSum(right)
  }

  // treeMap function
  def treeMap[T, V](f: T => V, tree: Tree[T]): Tree[V] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Node(value, left, right) => Node(f(value), treeMap(f, left), treeMap(f, right))
  }

  // BTreeMap function
  def BTreeMap(f: B => B, tree: Tree[B]): Tree[B] = treeMap(f, tree)

  // test method
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

    // Valid function calls
    println(BTreeMap(fab,myBTree))
    println(BTreeMap(fbb,myBTree))
    println(BTreeMap(fbc,myBTree))
    println(BTreeMap(fac,myBTree))
    println(treeMap(faa,myATree))
    println(treeMap(fab,myATree))
    println(treeMap(fac,myATree))

    // Commented out invalid function calls with explanations
    // BTreeMap calls
    // println(BTreeMap(faa,myBTree))	// Type error: faa expects A, but BTreeMap provides B
    // println(BTreeMap(fba,myBTree)) 	// Type error: fba expects B, returns A, but BTreeMap expects and returns B
    // println(BTreeMap(fcb,myBTree))	// Type error: fcb expects C, returns B, but BTreeMap provides B
    // println(BTreeMap(fcc,myBTree)) 	// Type error: fcc expects C, but BTreeMap provides B
    // println(BTreeMap(fca,myBTree)) 	// Type error: fca expects C, returns A, but BTreeMap provides B

    // treeMap calls
    // println(treeMap(fba,myATree)) 	// Type error: fba expects B, but treeMap is working with Tree[A]
    // println(treeMap(fbb,myATree)) 	// Type error: fbb expects B, but treeMap is working with Tree[A]
    // println(treeMap(fbc,myATree)) 	// Type error: fbc expects B, but treeMap is working with Tree[A]
    // println(treeMap(fcb,myATree)) 	// Type error: fcb expects C, but treeMap is working with Tree[A]
    // println(treeMap(fcc,myATree)) 	// Type error: fcc expects C, but treeMap is working with Tree[A]
    // println(treeMap(fca,myATree)) 	// Type error: fca expects C, but treeMap is working with Tree[A]
  }

  // main method
  def main(args: Array[String]): Unit = {
    test()
  }
}
