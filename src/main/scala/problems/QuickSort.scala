package problems

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{
  Await,
  ExecutionContext,
  ExecutionContextExecutor,
  Future
}
import scala.util.Random
import scala.concurrent.duration._
object QuickSort {
  val maxDepth: Int = 14
  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.global

  def randInclusive(start: Int, end: Int): Int =
    start + Random.nextInt(end - start + 1)
  def swap(arr: ArrayBuffer[Int], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  def partition(arr: ArrayBuffer[Int], start: Int, end: Int): Int = {
    val randIdx = randInclusive(start, end)
    swap(arr, randIdx, start)
    val partitionValue = arr(start)
    var partitionIdx = start
    for (i <- start to end) {
      val curr = arr(i)
      if (curr < partitionValue) {
        arr(i) = arr(partitionIdx + 1) // next to i
        arr(partitionIdx + 1) = arr(partitionIdx) // partition to next
        arr(partitionIdx) = curr // curr to partition
        partitionIdx += 1
      }
    }

    partitionIdx
  }

  def quickSortInPlace(arr: ArrayBuffer[Int], start: Int, end: Int): Unit = {
    if (start <= end) {
      val partitionIndex = partition(arr, start, end)
      quickSortInPlace(arr, start, partitionIndex - 1)
      quickSortInPlace(arr, partitionIndex + 1, end)
    }
    ()
  }

  def quickSort(arr: List[Int]): List[Int] = {
    if (arr.size < 2) arr
    else {
      val rand = arr(randInclusive(0, arr.size - 1))
      val left = quickSort(arr.filter(_ <= rand))
      val right = quickSort(arr.filter(_ > rand))
      left ++ right
    }
  }

  def merge(left: List[Int], right: List[Int]): List[Int] = {
    @tailrec
    def loop(xss: List[Int], yss: List[Int], res: List[Int]): List[Int] = {
      (xss, yss) match {
        case (x :: xs, y :: _) if x < y ⇒ loop(xs, yss, x :: res)
        case (x :: _, y :: ys) if x >= y ⇒ loop(xss, ys, y :: res)
        case (x :: xs, Nil) ⇒ loop(xs, Nil, x :: res)
        case (Nil, y :: ys) ⇒ loop(Nil, ys, y :: res)
        case (Nil, Nil) ⇒ res.reverse
      }
    }
    loop(left, right, Nil)
  }

  def mergeTwo(i: List[Int], j: List[Int]): List[Int] = {
    (i, j) match {
      case (Nil, Nil)     => Nil
      case (x :: xs, Nil) => i
      case (Nil, y :: ys) => j
      case (x :: xs, y :: ys) => {
        if (x <= y)
          x :: merge(i.tail, j)
        else
          y :: merge(i, j.tail)
      }
    }
  }

  def sortInParallelMerge(arr: List[Int], depth: Int): Future[List[Int]] = {
    if (arr.size < 2 || arr.size < depth || depth == maxDepth || arr.size / maxDepth == 0) {
      Future { quickSort(arr) }
    } else {
      val step = arr.size / maxDepth

      Future
        .sequence((0 to arr.size by step).map(start ⇒ {
          if (start + step > arr.size) {
            Future {
              arr.slice(start, arr.size).sorted
            }
          } else {
            Future {
              arr.slice(start, start + step).sorted
            }
          }
        }))
        .map(_.foldRight(List.empty[Int])((acc: List[Int], curr: List[Int]) ⇒
          merge(acc, curr)))
    }
  }

  def main(args: Array[String]): Unit = {
    (10000000 to 10000000 by 5).foreach(i ⇒ {
      val myList: List[Int] =
        (0 to i).map(_ ⇒ Random.nextInt(1000000000)).toList
      val arrayBuffer = ArrayBuffer(myList).flatten
      val start = System.currentTimeMillis()
      //      val res = Await.result(sortInParallelMerge(myList, 0), 20 seconds)
      quickSortInPlace(arrayBuffer, 0, arrayBuffer.size - 1)

//      val sorted = arrayBuffer.sorted

//      println(res)
//      println(sorted)
      println(System.currentTimeMillis() - start)
//      println(res equals sorted)
    })
  }
}
