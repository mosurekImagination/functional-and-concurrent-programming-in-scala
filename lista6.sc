//Tomasz Mosur 228068

//zadanie 1
def whileLoop(cond: => Boolean)(expr: => Unit): Unit =
  if (cond) {
    expr
    whileLoop(cond)(expr)
  }

var count = 0
whileLoop {
  count < 5
} {
  println(count)
  count += 1
}

//zadanie 2
//def getRepeatedStream[A](item:A, k: Int):Stream[A]=
//  if(k>0) item#::getRepeatedStream(item,k-1)
//  else Stream.Empty

def lrepeat[A](k: Int)(stream: Stream[A]): Stream[A] = {
  def helper[A](n: Int)(s: Stream[A]): Stream[A] =
    s match {
      case h #:: t =>
        if (n > 0) h #:: helper(n - 1)(s)
        else lrepeat(k)(t)
      case Stream.Empty => Stream.Empty
    }

  helper(k)(stream)
}
//  stream.flatMap(e => getRepeatedStream(e, k))
//  stream match {
//    case h#::t => getRepeatedStream(h,k)#:::lrepeat(k)(t)
//  }

(lrepeat(3)(Stream.from(1)) take 14).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5)
lrepeat(3)(Stream(1, 2, 3, 4)).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat(3)(Stream.Empty).toList == List()

//zadanie 3
sealed trait lBT[+A]

case object LEmpty extends lBT[Nothing]

case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

val t = LNode(1, () => LNode(2, () => LEmpty, () => LNode(3, () => LEmpty, () => LEmpty)), () => LEmpty)
val t2 = LNode(1, () => LNode(2, () => LEmpty, () => LEmpty), () => LNode(2, () => LEmpty, () => LEmpty))
//
def lBreadth[A](ltree: lBT[A]): Stream[A] = {
  def helper(queue: List[lBT[A]]): Stream[A] =
    queue match {
      case LNode(e, l, r) :: t => e #:: helper(t ::: l() :: r() :: Nil)
      case LEmpty :: t => helper(t)
      case Nil => Stream.Empty
    }

  helper(List(ltree))
}

//}
//
def lTree(n: Int): lBT[Int] = LNode[Int](n, () => lTree(2 * n), () => lTree(2 * n + 1))

lBreadth(LEmpty).toList == List()
lBreadth(t).toList == List(1, 2, 3)
(lBreadth(lTree(5)) take 10).toList == List(5, 10, 11, 20, 21, 22, 23, 40, 41, 42)




