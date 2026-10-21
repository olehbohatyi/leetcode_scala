#!/usr/bin/env -S scala shebang

// 15. 3Sum
// Difficulty: Medium
// https://leetcode.com/problems/3sum/

// Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]] such that
// i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.

// Notice that the solution set must not contain duplicate triplets.

// Example 1:
// Input: nums = [-1,0,1,2,-1,-4]
// Output: [[-1,-1,2],[-1,0,1]]
// Explanation:
// nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0.
// nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0.
// nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0.
// The distinct triplets are [-1,0,1] and [-1,-1,2].
// Notice that the order of the output and the order of the triplets does not matter.

// Example 2:
// Input: nums = [0,1,1]
// Output: []
// Explanation: The only possible triplet does not sum up to 0.

// Example 3:
// Input: nums = [0,0,0]
// Output: [[0,0,0]]
// Explanation: The only possible triplet sums up to 0.

// Constraints:
// 3 <= nums.length <= 3000
// -10^5 <= nums[i] <= 10^5

def threeSumWhile(nums: Array[Int]): List[List[Int]] =
  val numsSet = scala.collection.mutable.Set[List[Int]]()
  val seenMap = scala.collection.mutable.Map[Int, Int]()
  var i       = 0
  while i < nums.length do
    var j = i + 1
    while j < nums.length do
      seenMap
        .get(-(nums(i) + nums(j)))
        .fold(seenMap += (nums(j) -> j))(k =>
          if k != i && k != j then numsSet += List(nums(i), nums(j), nums(k)).sorted
          else seenMap += (nums(j) -> j)
        )
      j += 1
    i += 1

  numsSet.toList

  // -1, 0, 1, 2, -1, -4
  // -1, 0 k = 1, seen.cont(1) == false => next iter => add 0
  // -1, 1 k = 0

@main def threeSum(): Unit =

  val data = List(
    Array(-1, 0, 1, 2, -1, -4) -> List(List(-1, -1, 2), List(-1, 0, 1)),
    Array(0, 1, 1)             -> List.empty[List[Int]],
    Array(0, 0, 0)             -> List(List(0, 0, 0)),
    Array(1, -1, -1, 0, 0)     -> List(List(-1, 0, 1))
  )

  val impl = List(
    threeSumWhile
  )

  for
    threeSumDef    <- impl
    (input, check) <- data
  do
    assert:
      threeSumDef(input).toSet == check.toSet
