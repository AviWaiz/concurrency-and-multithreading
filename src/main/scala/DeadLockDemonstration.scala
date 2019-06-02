import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.Random

// to synchronize threads a in a critical section a few methods can be implemented
// however logical follies can be committed in an attempt to synchronize

// Deadlocks happen when threads are unable to make progress because the resource required by the one thread is held by another
// while another required resource in the other is held by the first

// liveness means a program will execute in a timely manner
// while deadlock code is not considered to exhibit liveness

// live lock is when threads take action in response to other threads instead of making any progress
// the best analogy is to think of two persons trying to cross each other in a hallway. one moves left to let the other pass
// the other moves right to let the first pass in response the first moves right now to let the second pass
// no progress is made since each blocks the other

object DeadLockDemonstration {
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global
}

class DeadLockDemonstration() {}

