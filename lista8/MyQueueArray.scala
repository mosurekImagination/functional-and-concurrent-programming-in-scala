package lista8

abstract class MyQueueArray[E] {
  @throws[FullException]
  def enqueue(x: E): Unit

  def dequeue: Unit

  @throws[NoSuchElementException]
  def first: E

  def isEmpty: Boolean

  def isFull: Boolean
}
