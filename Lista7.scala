
trait MyQueue[+T]{
  def empty():MyQueue[T]
  def enqueue[S >: T](e: S):MyQueue[S]
  def first():T
  def firstOption():Option[T]
  def dequeue():MyQueue[T]
  def isEmpty:Boolean
}

class QueueException(msg: String) extends Exception(msg)

private class MyQueueImpl[+T](private val out:List[T], private val in:List[T]) extends MyQueue[T]{

  def this()={
    this(List(),List())
  }

  override def empty() = new MyQueueImpl[T](List(),List())

  override def enqueue[S >: T](e: S): MyQueueImpl[S] = out match {
    case _::_ => new MyQueueImpl[S](out, e::in)
    case Nil => new MyQueueImpl[S] (e::in, List())
}

  override def first(): T = out match {
    case h::_ => h
    case Nil => throw new QueueException("Empty Queue")
  }

  override def firstOption(): Option[T] = out match {
    case h::_ => Some(h)
    case Nil => None
  }

  override def dequeue():MyQueueImpl[T] = out match {
    case _::Nil => new MyQueueImpl[T](in.reverse,List())
    case _::t => new MyQueueImpl[T](t,in)
    case Nil => this
  }

  override def isEmpty: Boolean = out==Nil

}

object MyQueue2{
  def apply[T](xs:T*):MyQueue[T] =
    new MyQueueImpl[T](xs.toList, List())
  def empty[T]:MyQueue[T] = apply[T]()

}

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

object Lista7 {

  def main(args: Array[String]): Unit = {
    print("first creation - MyQueue2[Int]() ")
    var firstQueueCreation = MyQueue2[Int]()
    firstQueueCreation = firstQueueCreation.enqueue(1)
    println(firstQueueCreation.first()==1)

    print("second creation - MyQueue.empty ")
    var secondQueueCreation = MyQueue2.empty[Int]
    secondQueueCreation = secondQueueCreation.enqueue(2)
    println(secondQueueCreation.first()==2)

    print("third creation - MyQueue(1,2) ")
    var thirdQueueCreation = MyQueue2(1,2)
    thirdQueueCreation = thirdQueueCreation.enqueue(3)
    println(thirdQueueCreation.first()==1)

    println("fourth creation - new MyQueueImpl")
    var fourthQueueCreation = new MyQueueImpl[Int]


    println("create queue")
    var queue = MyQueue2(1,2)
    println("take first elem")
    println(queue.first()==1)
    println("dequeue and take second elem")
    queue = queue.dequeue()
    println(queue.first()==2)
    println("create queue")
    queue = queue.dequeue()
    queue = queue.enqueue(5)
    queue = queue.enqueue(6)
    println(queue.first()==5)
    queue = queue.dequeue()
    println(queue.first()==6)
    queue = queue.dequeue()
    println(queue.isEmpty)
    queue = queue.enqueue(5)
    println("first option")
    queue.firstOption() == Option(5)
    queue = queue.dequeue()

    println("OPTION")
    queue.firstOption()
    queue.isEmpty
    println("OPTION2")
    try{
      queue.first()
    }catch {
      case e: Exception => print(e.getMessage)
    }
    var queue2 = MyQueue2().empty().enqueue(1).enqueue(2)
    println("Second queue")
    println(queue2.first()==1)
    println(queue2.first())
    queue2 = queue2.dequeue()
    println(queue2.first()==2)
    println(queue2.first())

    print("Tree travelsal tests")
    val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
    val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,
      Node(6,Empty,Empty)),Empty))

    println(breathBT(t))
    println("BreathBT 1,2,3")
    println(breathBT(t)== List(1, 2, 3))
    println(breathBT(Empty)==List())
    println("BreathBT 1,2,3,4,5,6")
    println(breathBT(tt)==List(1,2,3,4,5,6))
  }

  def breathBT[A](tree:BT[A]):List[A]= {
    def helper(toVisit: MyQueue[BT[A]]): List[A] =
      toVisit.firstOption() match {
        case Some(Node(e, l, r)) => e::helper(toVisit.enqueue(l).enqueue(r).dequeue())
        case Some(Empty) => helper(toVisit.dequeue())
        case None => Nil
      }
    helper(MyQueue2(tree))
  }

}
