package problems

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{Condition, ReentrantLock}

import scala.collection.mutable

// Asynchronous programming requires callback execution at a deferred time
// Design a thread safe callback class that allows callbacks to be registered and executed after user specified time interval

// One can create a busy thread that loops over a list of callbacks and executes them if they are due
// This is naive, however. Another solution is to have an execution thread that maintains a priority queue of callbacks
// ordered by the time remaining to execute.
// Execution thread can sleep for a duration before the earliest callback to execute in the min-heap is due
// Threads can register callbacks to the priority queue within a critical section
// The thread that sleeps according to the closest execution time will need to recalculate it's sleep
// whenever a new callback is registered


// there is a simple callback case class that holds information about execution time
// there is also  register callback method on the class that makes adds a callback to the queue
// reentrant lock used for guarding the critical section, which acts as a mutex also introduce a condition variable
// the execution thread wait on the condition while the thread adding the callback will signal

// lock before critical section
// all threads trying to register a callback will wait while another thread
// owns the ReentrantLock
// waiting threads are signaled in fifo order
// signal wakes up a a waiting thread on the condition
// the execution thread won't wake until the thread adding the callback gives up the lock


// start method
// thread enters a perpetual loop
// 1) initially queue will be empty and the execution thread should just wait
// indefinitely on the condition variable to be signalled

// 2) when the first callback arrives, we note how many seconds after its arrival does
// it need to be invoked and await() on the condition variable for that many seconds

// 3) Now two things are possible at this point. No new callbacks arrive, in which case the executor thread completes waiting
// and polls the queue for tasks that should be executed and starts executing them
// or a new callback arrives in which case the thread adding a callback would signal the condition variable newCallbackArrived
// to wake up the execution thread and have it re-evaluate the duration it can sleep for before the earliest callback becomes due
object DeferredCallbackExecutor {
  // PriorityQueue(min-heap) ordered by executeAt
  def callbackOrder(callback: CallBack): Long = callback.executeAt
  val q: mutable.PriorityQueue[CallBack] =
    mutable.PriorityQueue[CallBack]()(Ordering.by(callbackOrder))

  // Lock to guard critical sections
  val lock = new ReentrantLock()
  // Condition to make execution thread wait on
  val newCallbackArrived: Condition = lock.newCondition()

  def start() = {
    var sleepFor: Long = 0L
    var lastSeenQSize = 0

    while (true) {
      lock.lock() // get the lock at the start since there could be a callback in the queue

      while (q.isEmpty) {
        newCallbackArrived.await() // indefinitely wait while on the condition
      }

      if (lastSeenQSize == q.size) {
        newCallbackArrived.await(sleepFor, TimeUnit.MILLISECONDS)
      }

      // when enough time passed relative to the first element in the queue
      // get the callback and invoke it
      val currentTime: Long = System.currentTimeMillis()
      while (q.nonEmpty && currentTime >= q.head.executeAt) {
        val callBack: CallBack = q.dequeue()
        println("Executed at " + System.currentTimeMillis() / 1000 + " required at " + callBack.executeAt / 1000 + ": message:" + callBack.message)
      }
      sleepFor = if (q.isEmpty) 0 else q.head.executeAt - currentTime
      lastSeenQSize = q.size
      lock.unlock()
    }
  }

  def registerCallback(callBack: CallBack): Unit = {
    lock.lock()
    q.enqueue(callBack)
    newCallbackArrived.signal()
    lock.unlock()
  }
}

object CallBackFactory {
  def apply(executeAfter: Long, message: String): Unit = {
    CallBack(System.currentTimeMillis + executeAfter * 100, message)
  }
}

case class CallBack(executeAt: Long, message: String)
