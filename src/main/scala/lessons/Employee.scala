package lessons

class Employee {

  var name: String = ""
  val lock = new Object()
  def setName(newName: String): Unit = this.synchronized {
    name = newName
  }

  def resetName(newName: String): Unit = this.synchronized {
    name = ""
  }

  // method can be called even if one
  // of the other methods is currently running
  // since it is synchronized on a different monitor
  def getName: String= {
    lock.synchronized {
      name
    }
  }
}
