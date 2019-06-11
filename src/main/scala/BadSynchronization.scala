object BadSynchronization {
//  def main(args: Array[String]): Unit = {
//    val dummyObject: Object = new Object
//
//    // Attempting to call wait without it being surrounded with synchronized block
//    dummyObject.wait()
//  }
  def main(args: Array[String]): Unit = {
    val dummyObject: Object = new Object
    val lock: Object = new Object

    lock.synchronized {
      lock.notify()
      // Calling notify on object that is synchronized on another object
      dummyObject.wait()
    }

  }
}
