import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random
import scala.concurrent.duration._


// this example shows that two threads updating a value concurrently may cause inconsistencies
// and yield un expected results
object DemoThreadUnSafe {
  val random: Random = new Random(System.currentTimeMillis())
  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  def main(args: Array[String]): Unit = {
    val badCounter = new BadCounter

    val f1 = Future {
      (0 until 100).foreach(_ ⇒ badCounter.increment())
    }
    val f2 = Future {
      (0 until 100).foreach(_ ⇒ badCounter.decrement())
    }


    Await.result(f1, 10 seconds)
    Await.result(f2, 10 seconds)
    badCounter.printFinalCount
  }

}

class BadCounter(var count: Int = 0) {

  def increment(): Unit = {
    count += 1
    ()
  }
  def decrement(): Unit = {
    count -= 1
    ()
  }

  lazy val printFinalCount = println("counter is: " + count)
}
