package lessons

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random

// when this code runs randInt will print even though the two if statements contradict each other
// this is because a race condition that will set randInt while printer is checking the value
// this is the critical section in the code which means a piece of code that can be executed concurrently and exposes shard data
// with out thread synchronization of multiple threads executing a critical section a race condition can occur
object RaceConditionDemo {

  def main(args: Array[String]): Unit = {
    val raceConditionDemo: RaceConditionDemo = new RaceConditionDemo(Random.nextInt())
    implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
    val f1 = Future {
      raceConditionDemo.printer
    }
    val f2 = Future {
      raceConditionDemo.modifier
    }

    Await.result(f1, 10 seconds)
    Await.result(f2, 10 seconds)
  }
}

class RaceConditionDemo(var randInt: Int) {
  val random: Random = new Random(System.currentTimeMillis)

  def printer: Unit =
    (1000000 until 0 by -1).foreach { _ ⇒
      if (randInt % 5 == 0) {
        if (randInt % 5 != 0) {
          println(randInt)
        }
      }
    }

  def modifier: Unit =
    (1000000 until 0 by -1).foreach(_ ⇒ randInt = Random.nextInt(1000))

}
