//Tomasz Mosur 228068

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]
val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val t2 = Node(1, Node(2, Empty, Node(3, Empty, Node(5,Empty, Empty))), Empty)

println("zadanie 1")
//zadanie 1
def sumBT[A](bt: BT[Int]): Int =
  bt match {
    case Node(e, l, r) => e + sumBT(l) + sumBT(r)
    case Empty => 0
  }

sumBT(t) == 6
sumBT(Empty) == 0

println("zadanie 2")
//zadanie 2
def foldBt[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B =
  bt match {
    case Node(e, l, r) => f(e)(foldBt(f)(acc)(l), foldBt(f)(acc)(r))
    case Empty => acc
  }

println("zadanie 3")
//zadanie 3
//a
def sumBTfold[A](bt: BT[Int]): Int =
  foldBt((curr: Int) => (left: Int, right: Int) => curr + left + right)(0)(bt)

def mulBTfold[A](bt: BT[Int]): Int =
  foldBt((curr: Int) => (left: Int, right: Int) => curr * left * right)(1)(bt)

sumBT(t) == 6
sumBT(Empty) == 0
mulBTfold(t) == 6

//zadanie 3
//b
def inorderBTfold[A](bt: BT[A]): List[A] =
  foldBt((eval: A) => (left: List[A], right: List[A]) =>
    left ::: (eval :: right))(List[A]())(bt)

inorderBTfold(t) == List(2, 3, 1)
inorderBTfold(Empty) == List()
inorderBTfold(t2) == List(2,3,5,1)

println("zadanie 4")
//zadanie 4
def mapBT[A, B](f: A => B)(tree: BT[A]): BT[B] =
  foldBt[A, BT[B]]((eval: A) => (left, right) => Node(f(eval), left, right))(Empty)(tree)

mapBT((v: Int) => 2 * v)(t: BT[Int]) == Node(2, Node(4, Empty, Node(6, Empty, Empty)), Empty)
mapBT((v: Int) => 2 * v)(Empty: BT[Int]) == Empty
mapBT((v: Int) => -2 * v)(t2: BT[Int]) == Node(-2, Node(-4, Empty, Node(-6, Empty, Node(-10,Empty, Empty))), Empty)


println("zadanie 5")
// Zadanie 5
sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]
val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => List(5)
    case 4 => List(0, 2)
    case 5 => List(3)
    case n => throw
      new NoSuchElementException("Graph g: node" + n + "doesn't exist")
  })

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def search(visited: List[A])(toVisit: List[A]): Boolean =
    toVisit match {
      case h :: tail => (h==to) ||
        (if (visited contains h) search(visited)(tail)
        else search(h :: visited)(tail ::: (g succ h)))
      case Nil => false
    }
  search(Nil)(List(from))
}

pathExists(g)(4,1) == true
pathExists(g)(0,4) == false
pathExists(g)(1,2) == true
pathExists(g)(3, 0) == false