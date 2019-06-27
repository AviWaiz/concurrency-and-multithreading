package problems

import scala.annotation.tailrec
import scala.util.Random

object TESTS {

  def main(args: Array[String]): Unit = {
    for (i ← 0 to 30) {
      val start = System.currentTimeMillis()
      val a: List[Int] = (0 to 10000000).toList.map(_ ⇒ Random.nextInt(100000000))
//      val b = mergeSort(a, 0, a.size - 1)
      val b = a.sorted
      println(System.currentTimeMillis() - start)
      Thread.sleep(100)
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] = {
    @tailrec
    def loop(xss: List[Int], yss: List[Int], res: List[Int]): List[Int] = {
      (xss, yss) match {
        case (x :: xs, y :: _) if x < y ⇒ loop(xs, yss, x :: res)
        case (x :: _, y :: ys) if x >= y ⇒ loop(xss, ys, y :: res)
        case (Nil, y :: ys) ⇒ loop(Nil, ys, y :: res)
        case (x :: xs, Nil) ⇒ loop(xs, Nil, x :: res)
        case (Nil, Nil) ⇒ res.reverse
      }
    }
    loop(left, right, Nil)
  }

  def mergeSort(arr: List[Int], start: Int, end: Int): List[Int] = {
    if (start > end) List()
    else if (start == end) List(arr(start))
    else {
      val mid = start + (end - start) / 2
      val left = mergeSort(arr, start, mid)
      val right = mergeSort(arr, mid + 1, end)
      merge(left, right)
    }
  }

}
