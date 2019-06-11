package lessons

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object Demonstration {
  def main(args: Array[String]): Unit = {

    new SingleVsMultipleThreads().example()
  }
}

class SingleVsMultipleThreads(val sumTo: Long = 1000000000L,
                              val halfSums: ArrayBuffer[Long] =
                                ArrayBuffer(0L, 0L)) {
  implicit val executionContext: ExecutionContext = ExecutionContext.global
  def example() = {

    // example of executing runnable on a single thread
    val single: Thread = new Thread(() => {
      println(Range.Long(0L, sumTo, 1L).sum.toString)
    })

    val startTime: Long = System.currentTimeMillis()
    single.start() // executes the thread
    single.join() // awaits the result in the current thread

    println(
      "Execution time for single thread: " + (System
        .currentTimeMillis() - startTime))

    val startMultiple = System.currentTimeMillis

    val multiple1 = new Thread(() ⇒ {
      halfSums.update(0, Range.Long(0L, sumTo / 2L, 1L).sum)
    })

    val multiple2 = new Thread( () ⇒  {
      halfSums.update(1, Range.Long(sumTo / 2L, sumTo, 1L).sum)
    })
    multiple1.start()
    multiple2.start()

    multiple1.join()
    multiple2.join()


    println(
      "Execution time for two thread: " + (System
        .currentTimeMillis() - startMultiple))
    println("Combined sum of both the threads = " + (halfSums.sum)
    )

    // functional scala
    val scalaStart: Long = System.currentTimeMillis()
    val step: Long = sumTo / 10L
    val v: immutable.Seq[Long] = Await.result(Future
                           .sequence(Range.Long(0L, sumTo, step).map {
                             start: Long ⇒
                               Future {
                                 (start until (start + step)).sum
                               }
                           }),
                         10 seconds)
    println(
      "Execution time for two thread: " + (System
        .currentTimeMillis() - scalaStart))
    println("Combined sum of both the threads = " + v.sum)

  }
}
