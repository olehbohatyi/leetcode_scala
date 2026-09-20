#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec

// 4. Median of Two Sorted Arrays

// Given two sorted arrays nums1 and nums2 of size m and n respectively,
// return the median of the two sorted arrays.
// The overall run time complexity should be O(log (m+n)).

// Example 1:
// Input: nums1 = [1,3], nums2 = [2]
// Output: 2.00000
// Explanation: merged array = [1,2,3] and median is 2.

// Example 2:
// Input: nums1 = [1,2], nums2 = [3,4]
// Output: 2.50000
// Explanation: merged array = [1,2,3,4] and median is (2 + 3) / 2 = 2.5.

// Constraints:
// nums1.length == m
// nums2.length == n
// 0 <= m <= 1000
// 0 <= n <= 1000
// 1 <= m + n <= 2000
// -106 <= nums1[i], nums2[i] <= 106

def medianOfTwoSortedArraysSort(nums1: Array[Int], nums2: Array[Int]): Double = 
  val sorted = (nums1 ++ nums2).sorted
  val length = sorted.length
  val middle = length / 2
  if length % 2 == 0 then
    (sorted(middle - 1) + sorted(middle)) / 2.0
  else
    sorted(middle).toDouble

def medianOfTwoSortedArraysBinary(nums1: Array[Int], nums2: Array[Int]): Double =
  val (small, large) =
    if nums1.length <= nums2.length then (nums1, nums2)
    else (nums2, nums1)

  val length = small.length + large.length
  val middle = (length + 1) / 2

  var low = 0
  var len = small.length

  while low <= len do
    val partitionSmall = (low + len) / 2
    val partitionLarge = middle - partitionSmall

    val leftSmall =
      if partitionSmall == 0 then Int.MinValue
      else small(partitionSmall - 1)

    val rightSmall =
      if partitionSmall == small.length then Int.MaxValue
      else small(partitionSmall)

    val leftLarge =
      if partitionLarge == 0 then Int.MinValue
      else large(partitionLarge - 1)

    val rightLarge =
      if partitionLarge == large.length then Int.MaxValue
      else large(partitionLarge)

    if leftSmall <= rightLarge && leftLarge <= rightSmall then
      val leftMax = math.max(leftSmall, leftLarge)

      if length % 2 == 1 then
        return leftMax.toDouble

      val rightMin = math.min(rightSmall, rightLarge)
      return (leftMax.toDouble + rightMin) / 2.0

    if leftSmall > rightLarge then
      len = partitionSmall - 1
    else
      low = partitionSmall + 1

  throw IllegalArgumentException("Input arrays must be sorted")

def medianOfTwoSortedArraysFP(nums1: Array[Int], nums2: Array[Int]): Double =
  val length = nums1.length + nums2.length
  val middle = length / 2

  @tailrec
  def merge(
      index1: Int,
      index2: Int,
      step: Int,
      previous: Int,
      current: Int
  ): Double =
    if step > middle then
      if length % 2 == 0 then
        (previous.toDouble + current) / 2.0
      else
        current.toDouble
    else
      val takeFirst =
        index1 < nums1.length &&
          (index2 >= nums2.length || nums1(index1) <= nums2(index2))

      val next =
        if takeFirst then nums1(index1)
        else nums2(index2)

      merge(
        if takeFirst then index1 + 1 else index1,
        if takeFirst then index2 else index2 + 1,
        step + 1,
        current,
        next
      )

  merge(0, 0, 0, 0, 0)

@main def medianOfTwoSortedArrays(): Unit =
  
  val data = List(
    (Array(1, 3) -> Array(2))    -> 2.0,
    (Array(1, 2) -> Array(3, 4)) -> 2.5
  )

  val impl = List(
    medianOfTwoSortedArraysSort,
    medianOfTwoSortedArraysBinary,
    medianOfTwoSortedArraysFP
  )

  for
    medianOfTwoSortedArraysDef <- impl
    ((nums1, nums2), expected) <- data
  do
    assert:
      medianOfTwoSortedArraysDef(nums1, nums2) == expected
