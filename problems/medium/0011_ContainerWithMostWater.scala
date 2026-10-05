#!/usr/bin/env -S scala shebang

// 11. Container With Most Water
// Difficulty: Medium
// https://leetcode.com/problems/container-with-most-water/

// You are given an integer array height of length n. There are n vertical lines
// such that the two endpoints of the ith line are (i, 0) and (i, height(i)).
// Find two lines that, together with the x-axis, form a container that holds
// the most water. Return the maximum amount of water a container can store.
//
// The container must be formed by two lines and the x-axis. You may not slant
// the container.

// Example 1:
// Input: height = [1,8,6,2,5,4,8,3,7]
// Output: 49
// Explanation: The lines at indices 1 and 8 form a container with area
// min(8, 7) * (8 - 1) = 49.

// Example 2:
// Input: height = [1,1]
// Output: 1

// Constraints:
// n == height.length
// 2 <= n <= 10^5
// 0 <= height(i) <= 10^4

def containerWithMostWaterWhile(height: Array[Int]): Int =
  var l = 0
  var r = height.length - 1
  var res = 0

  while l < r do
    height(l) -> height(r) match
      case (lh, rh) if lh < rh =>
        val max = lh * (r - l)
        res = if res < max then max else res
        l += 1
      case (_, rh)             =>
        val max = rh * (r - l)
        res = if res < max then max else res
        r -= 1

  res

@main def containerWithMostWater(): Unit =

  val data = List(
    Array(1, 8, 6, 2, 5, 4, 8, 3, 7) -> 49,
    Array(1, 1) -> 1
  )

  val impl = List(
    containerWithMostWaterWhile
  )

  for
    containerWithMostWaterDef <- impl
    (height, expected) <- data
  do
    assert:
      containerWithMostWaterDef(height) == expected
