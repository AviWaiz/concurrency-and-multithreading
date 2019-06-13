package problems

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

//p = 0
//n = 0
//p = 0
//n = 1
//[3,4,5,2,4,3]
//p = 0
//n = 2
//[3,4,5,2,4,3]
//move next to smaller
//curr to next
//and small to curr
//[2,3,4,5,4,3]
//[2,3,3,4,5,4]

object QuickSort {

  def partition(arr: ArrayBuffer[Int], start: Int, end: Int): Int = {
    // pick a random element and move to front of range
    val randIdx = start + Random.nextInt((end - start) + 1) // inclusive random
    val partitionValue = arr(randIdx)
    var partitionIdx = start
    println("pre swap" + arr)
    println("partiion_idx" + arr)
    arr(randIdx) = arr(start)
    arr(start) = partitionValue
    println("swap " + arr)
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

  def main(args: Array[String]): Unit = {
    (0 to 0).foreach(_ ⇒ {
      val arrayBuffer: ArrayBuffer[Int] =
        ArrayBuffer((0 to 5).map(_ ⇒ Random.nextInt(100))).flatten
      val sorted = arrayBuffer.sorted
      println(arrayBuffer)
      println("Pre Sort")
      quickSort(arrayBuffer, 0, arrayBuffer.size - 1)
      println(arrayBuffer)
      println("Post Sort")
      println(sorted)
      println("sorted")
      println(arrayBuffer equals sorted)
    })
  }
}
