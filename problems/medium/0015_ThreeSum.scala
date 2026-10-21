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

def threeSumTwoPointers(nums: Array[Int]): List[List[Int]] =
  val sorted   = nums.sorted
  val triplets = scala.collection.mutable.ListBuffer[List[Int]]()
  var i        = 0

  while i < sorted.length - 2 do
    if i == 0 || sorted(i) != sorted(i - 1) then
      var left  = i + 1
      var right = sorted.length - 1

      while left < right do
        val sum = sorted(i) + sorted(left) + sorted(right)
        if sum == 0 then
          triplets += List(sorted(i), sorted(left), sorted(right))
          left += 1
          right -= 1
          while left < right && sorted(left) == sorted(left - 1) do left += 1
          while left < right && sorted(right) == sorted(right + 1) do right -= 1
        else if sum < 0 then left += 1
        else right -= 1
    i += 1

  triplets.toList

def threeSumHashSet(nums: Array[Int]): List[List[Int]] =
  val triplets = scala.collection.mutable.Set[List[Int]]()
  var i        = 0

  while i < nums.length - 2 do
    val seen = scala.collection.mutable.Set[Int]()
    var j    = i + 1

    while j < nums.length do
      val k = -(nums(i) + nums(j))
      if seen.contains(k) then triplets += List(nums(i), nums(j), k).sorted
      seen += nums(j)
      j += 1
    i += 1

  triplets.toList

def threeSumCounts(nums: Array[Int]): List[List[Int]] =
  val counts   = nums.groupMapReduce(identity)(_ => 1)(_ + _)
  val values   = counts.keys.toArray.sorted
  val triplets = scala.collection.mutable.ListBuffer[List[Int]]()
  var i        = 0

  while i < values.length do
    var j = i
    while j < values.length do
      val a = values(i)
      val b = values(j)
      val c = -(a + b)

      if c >= b && counts.contains(c) then
        val enoughOccurrences =
          if a == b && b == c then counts(a) >= 3
          else if a == b then counts(a) >= 2
          else if b == c then counts(b) >= 2
          else true

        if enoughOccurrences then triplets += List(a, b, c)
      j += 1
    i += 1

  triplets.toList

@main def threeSum(): Unit =

  val data = List(
    Array(-1, 0, 1, 2, -1, -4)    -> List(List(-1, -1, 2), List(-1, 0, 1)),
    Array(0, 1, 1)                -> List.empty[List[Int]],
    Array(0, 0, 0)                -> List(List(0, 0, 0)),
    Array(1, -1, -1, 0, 0)        -> List(List(-1, 0, 1)),
    Array(-2, 0, 0, 2, 2)         -> List(List(-2, 0, 2)),
    Array(-4, -2, -2, 0, 2, 2, 4) -> List(
      List(-4, 0, 4),
      List(-4, 2, 2),
      List(-2, -2, 4),
      List(-2, 0, 2)
    )
  )

  val impl = List(
    threeSumWhile,
    threeSumTwoPointers,
    threeSumHashSet,
    threeSumCounts
  )

  for
    threeSumDef    <- impl
    (input, check) <- data
  do
    assert:
      threeSumDef(input).toSet == check.toSet
