import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

object IncorrectSynchronization {
  def main(args: Array[String]): Unit = {
    val incorrectSynchronization = new IncorrectSynchronization
    incorrectSynchronization.example()
  }
}

class IncorrectSynchronization() {
  var flag = true
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global

  def example(): Unit = {
    val t1: Thread = new Thread(() ⇒ {
      flag.synchronized {
        try {
          while (flag) {
            println("First thread about to sleep")
            Thread.sleep(5000)
            println("Woke up and about to invoke wait()")
            println(flag)
            flag.wait()
            println("After wait")
          }
        } catch {
          case ie: InterruptedException ⇒ println(ie)
        }
      }
    })


    val t2: Thread = new Thread(() ⇒ {
      flag = false
      println("Boolean assignment done.")
    })
    ()
    t1.start()
    Thread.sleep(1000)
    t2.start()
    t1.join()
    t2.join()
  }
}
