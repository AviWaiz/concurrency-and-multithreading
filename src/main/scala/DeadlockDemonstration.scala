import java.util.concurrent.CountDownLatch

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
object DeadlockDemonstration {
  def main(args: Array[String]): Unit = {
    //    val dead = new DeadlockDemonstration
    //    Try(dead.runTest) match {
    //      case Success(value) ⇒ println("Success")
    //      case Failure(exception) ⇒ println("Failed")
    //    }

    val nreLock = new NonReentrantLock()
    nreLock.lock()
    print("Got firstLock")
    // Attempt to get second lock will deadlock self
    println("Getting second lock")
    nreLock.lock()
    println("GotEEM")
  }

}

class DeadlockDemonstration() {
  private var counter: Int = 0
  private val lock1 = new Object()
  private val lock2 = new Object()
  val latch = new CountDownLatch(2)
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global

  val f1: Runnable = () =>
    (0 until 100).foreach(i ⇒ {
      incrementCounter()
      println("Incrementing " + i)
    })

  val f2: Runnable = () ⇒ {
    (0 until 100).foreach(i ⇒ {
      decrementCounter()
      println("Incrementing " + i)
    })
  }

  def runTest(): Unit = {
    val t1 = new Thread(f1)
    val t2 = new Thread(f2)
    t1.start()
    Thread.sleep(100)
    t2.start()
    t1.join()
    t2.join()
    println("Done : " + counter)
  }

  def incrementCounter(): Unit = {
    lock1.synchronized {
      latch.countDown()
      println("Acquired lock1")
      latch.await()
      lock2.synchronized {
        counter += 1
      }
    }
    ()
  }
  def decrementCounter(): Unit = {
    lock2.synchronized {
      println("Acquired lock2")
      latch.countDown()
      latch.await()
      lock1.synchronized {
        counter -= 1
      }
    }
    ()
  }
}

class NonReentrantLock(var isLocked: Boolean = false) {

  def lock(): Unit = synchronized {
    while(isLocked) {
      wait()
    }
    isLocked = true
  }

  def unlock(): Unit = synchronized {
    isLocked = false
    notify()
  }


}
