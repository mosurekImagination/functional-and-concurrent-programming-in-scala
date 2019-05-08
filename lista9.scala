import java.util.concurrent.Semaphore

//TOMASZ MOSUR 228068
object Zad1a extends App {
  var counter = 0 // counter variable
  
  def readWriteCounter(): Unit = {
      val incrementedCounter = counter + 1 // reading counter
      counter = incrementedCounter // writing to counter
      // counter += 1 // shorter code
  }
  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val startTime = System.nanoTime
  p.start; q.start
  p.join; q.join
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}
object Zad1b extends App {
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = {
    this.synchronized {
      val incrementedCounter = counter + 1 // reading counter
      counter = incrementedCounter // writing to counter
      // counter += 1 // shorter code
    }
  }
  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val startTime = System.nanoTime
  p.start; q.start
  p.join; q.join
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}
object Zad1c extends App {
  var counter = 0 // counter variable

  var semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
      semaphore.acquire()
      val incrementedCounter = counter + 1 // reading counter
      counter = incrementedCounter // writing to counter
      semaphore.release()
    }
  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val startTime = System.nanoTime
  p.start; q.start
  p.join; q.join
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

object Lista9Zadanie23 {

  def parallel[A, B](block1: => A, block2: => B): (A, B) = {

    var a : Option[A]= None
    var b : Option[B] = None

    val q = new Thread(() => a=Some(block1))
    val p = new Thread(() => b=Some(block2))

    p.start(); q.start()
    p.join(); q.join()

    (a.get, b.get)
  }

  def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
    val thr = new Thread(() =>
      for (_ <- 0 to times) {
        block
        Thread.sleep(duration)
      })
    thr.setDaemon(true)
    thr.start()
  }

  def main(args: Array[String]): Unit = {
    println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
    println(parallel({2+3}, {2}))
    println(parallel({2+3}, parallel({4*5}, {2})))

    println()
    println()

    periodically(1000, 5)(print("y "))
    periodically(1000, 25)(print("x "))
    Thread.sleep(10000)
    println("Done sleeping")
  }
}