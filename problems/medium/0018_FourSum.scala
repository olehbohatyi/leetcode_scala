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

@main def fourSum(): Unit =

  val data = List(
    (Array(1, 0, -1, 0, -2, 2), 0) -> List(List(-2, -1, 1, 2), List(-2, 0, 0, 2), List(-1, 0, 0, 1)),
    (Array(2, 2, 2, 2, 2), 8)      -> List(List(2, 2, 2, 2))
  )

  val impl = List(
    fourSumPointers
  )

  for
    problem4SumDef            <- impl
    ((nums -> target), check) <- data
  do
    assert:
      problem4SumDef(nums, target) == check
