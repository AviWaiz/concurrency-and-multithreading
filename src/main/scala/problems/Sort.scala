package problems

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random
import scala.concurrent.duration._

object Sort {
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global
  val maxDepth: Int = Runtime.getRuntime.availableProcessors()
  def main(args: Array[String]): Unit = {
    (1000000 to 1000000).foreach { i ⇒
      val vec = (0 to i).toList.map((_: Int) ⇒ Random.nextInt(100000000))
//      val a = (0 to i).toVector.map((_: Int) ⇒ Random.nextInt(10000000))
      val start = System.currentTimeMillis()
      val res = mergeSort(vec, 0, vec.size - 1)
      println("Time mergeSort " + (System.currentTimeMillis() - start))
      val start2 = System.currentTimeMillis()
      val res2 =
        Await.result(mergeSortParallel(vec, 0, vec.size - 1, 0), 60 seconds)
      println("Time mergeSortParallel " + (System.currentTimeMillis() - start2))
//      val start3 = System.currentTimeMillis()
//      val res3 =
//        Await.result(parallelSort(vec, 0, vec.size - 1, 0), 60 seconds)
//      println("Time parallelSort " + (System.currentTimeMillis() - start3))
//      val start4 = System.currentTimeMillis()
//      val res4 = vec.sorted
//      println("Time scalaSort " + (System.currentTimeMillis() - start4))
//      println(res3 == res4)
//      println(res3)
//      println(res2 == res4)
//      println(res == res4)
    }
  }

  def partition(arr: ArrayBuffer[Int], start: Int, end: Int): Int = {
    // pick a random element and move to front of range
    val randIdx = start + Random.nextInt((end - start) + 1) // inclusive random
    val partitionValue = arr(randIdx)
    var partitionIdx = start
    arr(randIdx) = arr(start)
    arr(start) = partitionValue
    (start until end).foreach(i ⇒ {
      val curr = arr(i)
      if (curr < partitionValue) {
        arr(i) = arr(partitionIdx + 1) // next to i
        arr(partitionIdx) = curr // curr to partition
        arr(partitionIdx + 1) = partitionValue // partition to next
        partitionIdx = i
      }
    })

    partitionIdx
  }

  def quickSort(arr: ArrayBuffer[Int],
                start: Int,
                end: Int): Unit = {
    println("start " + start)
    println("end " + end)
    if (start >= end) ()
    else {
      val p = partition(arr, start, end)
      quickSort(arr, start, p - 1)
      quickSort(arr, p, end)
    }
  }


  def parallelSort(values: List[Int],
                   start: Int,
                   end: Int,
                   depth: Int): Future[List[Int]] = {
    if (start > end) Future { List() } else if (depth > maxDepth || start + end < depth || start == end) {
      Future { values.sorted }
    } else {
      val step = values.size / maxDepth
      Future
        .sequence(
          (0 to values.size + 1 by step).map(start ⇒
            if (start + step >= values.size)
              Future {
                values.slice(start, values.size).sorted
              } else Future { values.slice(start, start + step) }))
        .map(_.flatten.sorted.toList)
    }
  }

  def mergeSortParallel(values: List[Int],
                        start: Int,
                        end: Int,
                        depth: Int): Future[List[Int]] = {
    if (start > end) Future { List() } else if (depth > maxDepth / 2 || start + end < depth || start == end) {
      Future { mergeSort(values, start, end) }
    } else {
      val middle = (end - start) / 2 + start
      val f1: Future[List[Int]] =
        mergeSortParallel(values, start, middle, depth + 1)
      val f2: Future[List[Int]] =
        mergeSortParallel(values, middle + 1, end, depth + 1)
      f1.flatMap(
          l1 ⇒
            f2.map(l2 ⇒ merge(l1, l2)))
    }
  }

  def mergeSort(values: List[Int], start: Int, end: Int): List[Int] = {
    if (start > end) List()
    else if (start == end) List(values(start))
    else {
      val mid: Int = (end - start) / 2 + start
      val left: List[Int] = mergeSort(values, start, mid)
      val right: List[Int] = mergeSort(values, mid + 1, end)
      merge(left, right)
    }
  }

  // idiomatic scala implementation
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
}
