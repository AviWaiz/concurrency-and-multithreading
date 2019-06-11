package problems

import java.util.concurrent.{
  ArrayBlockingQueue,
  Executors,
  ThreadPoolExecutor,
  TimeUnit
}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Random
// Queue that blocks enqueue when full and dequeue when empty
// this can be used to prevent a machine from running out of resources as a collection of jobs gets larger
// Queue should notify enqueueing about availability and similar for dequeueing
// have to synchronize on size checks since many threads could be checking at the same time and causing race conditions
//

// Queue for how many workers can run limit number of jobs
// limit number of workers
object BlockingQueue {
  val numJobs = 100
  val numWorkers = 6
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global

  val jobQueue: ArrayBlockingQueue[Runnable] =
    new ArrayBlockingQueue[Runnable](numJobs) {
      override def offer(e: Runnable): Boolean = {
        put(e); // may block if waiting for empty room
        true
      }
    }

  def main(args: Array[String]): Unit = {
    // ThreadPoolExecutor
    // corePoolSize, maxPoolSize, timeout, timeoutUnit, workQueue
    val tpe: ThreadPoolExecutor = new ThreadPoolExecutor(numWorkers,
      numWorkers,
      100L,
      TimeUnit.SECONDS,
      jobQueue)
    val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(tpe)

    val start = System.currentTimeMillis()

    Future.sequence(
      (1 to 400).map(_ ⇒
        Future {
          (0 to 1000000).toList.map(_ ⇒ Random.nextInt(1000)).sorted
          println("Size 1: " + tpe.getQueue.size())
        }(ec))
    )
  }
}

object BlockingQueueJavaLike {

  def main(args: Array[String]): Unit = {
    val blockingQueue: BlockingQueueJavaLike[Int] =
      new BlockingQueueJavaLike[Int](Vector.empty[Int], new Object, 5)

    val t1: Thread = new Thread(() ⇒ {
      try {
        (0 to 50).foreach(i ⇒ {
          blockingQueue.enqueue(i)
          println("enqueued " + i)
        })
      } catch {
        case e: InterruptedException ⇒ println("InterruptedException")
      }
    })
    val t2: Thread = new Thread(() ⇒ {
      try {
        (0 to 25).foreach(i ⇒ {
          println("Thread 2 dequeue: " + blockingQueue.dequeue())
        })
      } catch {
        case e: InterruptedException ⇒ println("InterruptedException")
      }
    })
    val t3: Thread = new Thread(() ⇒ {
      try {
        (0 to 25).foreach(i ⇒ {
          println("Thread 3 dequeue: " + blockingQueue.dequeue())
        })
      } catch {
        case e: InterruptedException ⇒ println("InterruptedException")
      }
    })

    t1.start
    Thread.sleep(4000)
    t2.start

    t2.join

    t3.start
    t1.join
  }
}

class BlockingQueueJavaLike[T](var queue: Vector[T],
                               lock: Object,
                               capacity: Int) {
  def enqueue(item: T): Unit = {
    lock.synchronized {
      while (queue.size == capacity) lock.wait()
      queue = queue :+ item
      lock.notify()
    }
  }

  def dequeue(): Option[T] = {
    var item: Option[T] = None
    lock.synchronized {
      while (queue.isEmpty) lock.wait()
      item = Some(queue.head)
      queue = queue.tail
      lock.notify()
    }
    item
  }
}
