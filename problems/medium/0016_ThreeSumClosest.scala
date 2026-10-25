#!/usr/bin/env -S scala shebang

// 16. 3Sum Closest
// Difficulty: Medium
// https://leetcode.com/problems/3sum-closest/

// You are given an integer array nums of length n and an integer target.
// Find three integers at distinct indices in nums such that the sum is
// closest to target.
// Return the sum of the three integers.
// You may assume that each input would have exactly one solution.

// Example 1:
// Input: nums = [-1,2,1,-4], target = 1
// Output: 2
// Explanation: -1 + 2 + 1 = 2 is closest to 1.

// Example 2:
// Input: nums = [0,0,0], target = 1
// Output: 0
// Explanation: The sum that is closest to the target is 0. (0 + 0 + 0 = 0).

// Constraints:
// 3 <= nums.length <= 500
// -1000 <= nums[i] <= 1000
// -10^4 <= target <= 10^4

def threeSumClosestBruteForce(nums: Array[Int], target: Int): Int =
  var closest  = nums(0) + nums(1) + nums(2)
  var bestDiff = math.abs(closest - target)
  var i        = 0

  while i < nums.length - 2 do
    var j = i + 1
    while j < nums.length - 1 do
      var k = j + 1
      while k < nums.length do
        val sum  = nums(i) + nums(j) + nums(k)
        val diff = math.abs(sum - target)
        if diff < bestDiff then
          closest = sum
          bestDiff = diff
        k += 1
      j += 1
    i += 1

  closest

def threeSumClosestTwoPointers(nums: Array[Int], target: Int): Int =
  val sorted  = nums.sorted
  var closest = sorted(0) + sorted(1) + sorted(2)
  var i       = 0

  while i < sorted.length - 2 do
    var l = i + 1
    var r = sorted.length - 1

    while l < r do
      val sum = sorted(i) + sorted(l) + sorted(r)
      if math.abs(sum - target) < math.abs(closest - target) then closest = sum

      if sum < target then l += 1
      else if sum > target then r -= 1
      else return sum
    i += 1

  closest

def threeSumClosestScan(nums: Array[Int], target: Int): Int =
  val sorted  = nums.sorted
  var closest = sorted(0) + sorted(1) + sorted(2)
  var i       = 0

  while i < sorted.length - 2 do
    var left  = i + 1
    var right = sorted.length - 1

    while left < right do
      val sum  = sorted(i) + sorted(left) + sorted(right)
      val diff = math.abs(sum - target)
      if diff < math.abs(closest - target) then closest = sum

      if sum < target then left += 1
      else if sum > target then right -= 1
      else return sum
    i += 1

  closest

@main def threeSumClosest(): Unit =

  val data = List(
    (Array(-1, 2, 1, -4), 1)                 -> 2,
    (Array(0, 0, 0), 1)                      -> 0,
    (Array(1, 1, 1, 0), -100)                -> 2,
    (Array(1, 2, 4, 8, 16, 32, 64, 128), 82) -> 82,
    (Array(-5, -2, -1, 4, 8), 3)             -> 2
  )

  val impl = List(
    threeSumClosestBruteForce,
    threeSumClosestTwoPointers,
    threeSumClosestScan
  )

  for
    threeSumClosestDef      <- impl
    ((nums, target), check) <- data
  do
    assert:
      threeSumClosestDef(nums, target) == check
