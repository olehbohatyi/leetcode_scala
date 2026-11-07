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

private def twoSum(sorted: Array[Int], start: Int, target: Int): List[List[Int]] =
  val counts = scala.collection.mutable.HashMap.empty[Int, Int]
  var index  = start
  while index < sorted.length do
    counts.update(sorted(index), counts.getOrElse(sorted(index), 0) + 1)
    index += 1

  val pairs = List.newBuilder[List[Int]]
  index = start
  while index < sorted.length do
    val left = sorted(index)
    if index == start || left != sorted(index - 1) then
      val right = target - left
      if right >= left && counts.get(right).exists(count => right != left || count > 1) then pairs += List(left, right)
    index += 1
  pairs.result()

def fourSumHash(nums: Array[Int], target: Int): List[List[Int]] =
  val sorted  = nums.sorted
  val results = List.newBuilder[List[Int]]
  var i       = 0

  while i <= sorted.length - 4 do
    if i == 0 || sorted(i) != sorted(i - 1) then
      var j = i + 1
      while j <= sorted.length - 3 do
        if j == i + 1 || sorted(j) != sorted(j - 1) then
          val pairTarget = target - sorted(i) - sorted(j)
          twoSum(sorted, j + 1, pairTarget)
            .foreach(pair => results += (sorted(i) :: sorted(j) :: pair))
        j += 1
    i += 1

  results.result()

private def nSum(nums: Array[Int], k: Int, target: Int): List[List[Int]] =
  val sorted = nums.sorted

  def search(start: Int, count: Int, remaining: Int): List[List[Int]] =
    if count < 2 || sorted.length - start < count then Nil
    else if count == 2 then twoSum(sorted, start, remaining)
    else
      val results = List.newBuilder[List[Int]]
      var i       = start

      while i <= sorted.length - count do
        if i == start || sorted(i) != sorted(i - 1) then
          search(i + 1, count - 1, remaining - sorted(i))
            .foreach(tail => results += (sorted(i) :: tail))
        i += 1

      results.result()

  search(0, k, target)

def fourSumNSum(nums: Array[Int], target: Int): List[List[Int]] =
  nSum(nums, 4, target)

@main def fourSum(): Unit =

  val data = List(
    (Array(1, 0, -1, 0, -2, 2), 0) -> List(List(-2, -1, 1, 2), List(-2, 0, 0, 2), List(-1, 0, 0, 1)),
    (Array(2, 2, 2, 2, 2), 8)      -> List(List(2, 2, 2, 2)),
    // TODO
    (Array(1000000000, 1000000000, 1000000000, 1000000000), -294967296) -> List(
      List(1000000000, 1000000000, 1000000000, 1000000000)
    )
  )

  val impl = List(
    fourSumHash,
    fourSumNSum
  )

  for
    problem4SumDef            <- impl
    ((nums -> target), check) <- data
  do
    assert:
      problem4SumDef(nums, target) == check
