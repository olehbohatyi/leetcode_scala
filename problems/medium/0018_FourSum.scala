#!/usr/bin/env -S scala shebang

// 18. 4Sum
// Difficulty: Medium
// https://leetcode.com/problems/4sum/

// Given an array `nums` of `n` integers, return an array of all the unique
// quadruplets `[nums[a], nums[b], nums[c], nums[d]]` such that:
// `0 <= a, b, c, d < n`
// `a`, `b`, `c`, and `d` are distinct.
// `nums[a] + nums[b] + nums[c] + nums[d] == target`

// You may return the answer in any order.

// Example 1:
// Input: nums = [1,0,-1,0,-2,2], target = 0
// Output: [[-2,-1,1,2],[-2,0,0,2],[-1,0,0,1]]

// Example 2:
// Input: nums = [2,2,2,2,2], target = 8
// Output: [[2,2,2,2]]

// Constraints:
// - 1 <= nums.length <= 200
// - -10^9 <= nums[i] <= 10^9
// - -10^9 <= target <= 10^9

def fourSumPointers(nums: Array[Int], target: Int): List[List[Int]] =
  val sorted = nums.sorted
  val n      = sorted.length
  var result = Set[List[Int]]()

  for i <- 0 until n - 3 do
    for j <- i + 1 until n - 2 do
      var left  = j + 1
      var right = n - 1

      while left < right do
        val sum = sorted(i) + sorted(j) + sorted(left) + sorted(right)
        if sum == target then
          result += List(sorted(i), sorted(j), sorted(left), sorted(right)).sorted
          left += 1
          right -= 1
        else if sum < target then left += 1
        else right -= 1

  result.toList

private def nSum(nums: Array[Int], k: Int, target: Long): List[List[Int]] =
  val sorted = nums.sorted

  def rangeSum(from: Int, count: Int): Long =
    var sum = 0L
    var i   = from
    while i < from + count do
      sum += sorted(i)
      i += 1
    sum

  def search(start: Int, count: Int, remaining: Long): List[List[Int]] =
    if count < 2 || sorted.length - start < count then Nil
    else if remaining < rangeSum(start, count) ||
      remaining > rangeSum(sorted.length - count, count)
    then Nil
    else if count == 2 then
      val pairs = List.newBuilder[List[Int]]
      var left  = start
      var right = sorted.length - 1

      while left < right do
        val sum = sorted(left).toLong + sorted(right).toLong
        if sum == remaining then
          pairs += List(sorted(left), sorted(right))
          val leftValue  = sorted(left)
          val rightValue = sorted(right)
          while left < right && sorted(left) == leftValue do left += 1
          while left < right && sorted(right) == rightValue do right -= 1
        else if sum < remaining then left += 1
        else right -= 1

      pairs.result()
    else
      val results = List.newBuilder[List[Int]]
      var i       = start

      while i <= sorted.length - count do
        if i == start || sorted(i) != sorted(i - 1) then
          search(i + 1, count - 1, remaining - sorted(i).toLong)
            .foreach(tail => results += (sorted(i) :: tail))
        i += 1

      results.result()

  search(0, k, target)

def fourSumNSum(nums: Array[Int], target: Int): List[List[Int]] =
  nSum(nums, 4, target.toLong)

@main def fourSum(): Unit =

  val data = List(
    (Array(1, 0, -1, 0, -2, 2), 0) -> List(List(-2, -1, 1, 2), List(-2, 0, 0, 2), List(-1, 0, 0, 1)),
    (Array(2, 2, 2, 2, 2), 8)      -> List(List(2, 2, 2, 2)),
    // TODO
    // (Array(1000000000, 1000000000, 1000000000, 1000000000), -294967296) -> List(
    //   List(1000000000, 1000000000, 1000000000, 1000000000)
    // )
  )

  val impl = List(
    fourSumPointers,
    fourSumNSum
  )

  for
    problem4SumDef            <- impl
    ((nums -> target), check) <- data
  do
    assert:
      problem4SumDef(nums, target) == check
