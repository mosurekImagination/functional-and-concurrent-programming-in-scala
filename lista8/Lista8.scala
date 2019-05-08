package lista8

import scala.reflect.ClassTag

//Tomasz Mosur 228068

class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueueArray[E] {

  private val size = capacity + 1
  private val queue: Array[E] = new Array[E](size)
  private var front, rear = 0

  override def enqueue(x: E): Unit =
    if (isFull) throw new FullException("Full exception")
    else {
      queue(rear) = x
      rear = (rear + 1) % size
    }

  override def dequeue: Unit =
    if (!isEmpty) front = (front + 1) % size

  override def first: E =
    if (isEmpty) throw new NoSuchElementException("First on empty queue")
    else queue(front)

  override def isEmpty: Boolean =
    front == rear

  override def isFull: Boolean =
    (rear + 1) % size == front
}

object MyQueue3 {
  def apply[E: ClassTag](xs: E*): QueueMut[E] = {
    var queue: QueueMut[E] = MyQueue3.empty()
    xs.foreach(x => queue.enqueue(x))
    queue
  }

  def empty[E: ClassTag](capacity: Int = 1000): QueueMut[E] = new QueueMut[E](capacity)
}

object Lista8 {

  def main(args: Array[String]): Unit = {
    var q = new QueueMut[Int](3)
    q.enqueue(1)
    q.enqueue(2)
    println(q.first == 1)
    q.dequeue
    println(q.first == 2)
    q.dequeue
    println(q.isEmpty)
    q.enqueue(4)
    q.enqueue(5)
    q.enqueue(6)
    println(q.isFull)
    q.dequeue
    println(!q.isFull)
    q.enqueue(7)
    println(q.isFull)

    var q2 : QueueMut[Int] = MyQueue3.empty(3)
    q2.enqueue(5)
    println(q2.first==5)

    var q3 = MyQueue3(1, 2, 3)
    println(q3.first==1)
    var q4 = MyQueue3()

  }
}
