package problems

import java.util.concurrent.locks.{Condition, ReentrantLock}

import scala.collection.mutable

// Asynchronous programming requires callback execution at a deferred time
// Design a thread safe callback class that allows callbacks to be registered and executed after user specified time interval

// One can create a busy thread that loops over a list of callbacks and executes them if they are due
// This is naive, however. Another solution is to have an execution thread that maintains a priority queue of callbacks
// ordered by the time remaining to execute.
// Execution thread can sleep for a duration before the earliest callback to execute in the min-heap is due
// Threads can register callbacks to the priority queue within a critical section

object DeferredCallbackExecutor {
  // PriorityQueue(min-heap) ordered by executeAt
  def callbackOrder(callback: CallBack): Long = callback.executeAt
  val q: mutable.PriorityQueue[CallBack] =
    mutable.PriorityQueue[CallBack]()(Ordering.by(callbackOrder))

  // Lock to guard critical sections
  val lock = new ReentrantLock()
  // Condition to make execution thread wait on
  val newCallbackArrived: Condition = lock.newCondition()

  def start() = {}

  def registerCallback(callBack: CallBack): Unit = {
    // lock before critical section
    // all threads trying to register a callback will wait while another thread
    // owns the ReentrantLock
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
