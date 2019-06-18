package problems
import java.util.concurrent.Executors

import scala.concurrent.{
  ExecutionContext,
  ExecutionContextExecutor,
  Future,
  blocking
}
// Bucket gets filled at constant rate, and has max capacity
// create a class that is thread safe and lets threads get a token when one is available
// token threads should block if no items in the bucket
// this is just a blocking queue
// should expose an API called getToken that various threads can call to get a token

// since the push rate is constant one must only keep track of time since
// last request came in. This will determine the possible number of tokens
// available to query.
// In the class keep value for the last time a request for tokens was made
// three scenarios:
// last req was > 5 seconds ago, so simply set max available to max size of the bucket
// last req was < 5 seconds ago, so find the number of tokens added relative to the number of unused tokens also pull one token from the count
// last req was < 5 seconds ago, but all the tokens are used up, so sleep for one second to have one more token added. when the sleep is happening the monitor would still be held by the requesting thread and new threads invoking getToken will be blocked waiting for the monitor

// for this problem a token generating thread is not necessary
// this is simply because the rate tokens are added is constant
// thread safety can be achieved with synchronized with getToken method
// can use a code block to achieve the same effect but this is not necessary
//
object TokenBucketFilter {

  implicit val ec =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  def main(args: Array[String]): Unit = this.synchronized {
    // Scala style
    // blocking code blocks should be executed in a separate execution context
    // https://github.com/alexandru/scala-best-practices/blob/master/sections/4-concurrency-parallelism.md#44-must-use-scalas-blockcontext-on-blocking-io
    val tokenBucketFilter = new TokenBucketFilter(1)
    val allFutures: Map[String, Future[Unit]] = (0 to 10).map(i ⇒
      ("Thread_" + (i + 1), Future {
        blocking {
          tokenBucketFilter.getToken()
        }
        ()
    }.recover({case _: Exception ⇒ println("We have a problem")}))).toMap
    // Java Style
//    val allThreads: Set[Thread] = (0 to 10).map(i ⇒ {
//      val t = new Thread(() ⇒ {
//        try {
//          tokenBucketFilter.getToken()
//        } catch {
//          case _: Throwable ⇒ println("We have a problem")
//        }
//      })
//      t.setName("Thread_" + (i + 1))
//      t
//    }).toSet
//    allThreads.foreach(_.start())
//    allThreads.foreach(_.join())
  }
}

class TokenBucketFilter(MAX_TOKENS: Int,
                        var lastRequestTime: Long = System.currentTimeMillis(),
                        var possibleTokens: Long = 0) {

  def getToken(): Unit = this.synchronized {
    // Add to the number of available tokens since the last request happened in seconds
    possibleTokens += (System.currentTimeMillis() - lastRequestTime) / 1000
    if (possibleTokens > MAX_TOKENS) {
      possibleTokens = MAX_TOKENS // Bucket is full
    } else if (possibleTokens == 0) {
      // All threads have a token
      // sleep one second to guarantee a token
      Thread.sleep(1000)
    } else {
      // provide a thread with a token
      possibleTokens -= 1
    }
    // Update the last time a token was requested
    lastRequestTime = System.currentTimeMillis()
    println(
      "Granting " + Thread.currentThread.getName + " token at " + (System.currentTimeMillis / 1000))
    ()
  }

}
