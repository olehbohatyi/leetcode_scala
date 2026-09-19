import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.util.boundary, boundary.break

//  1. Two Sum
//
//  Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
//
//  You may assume that each input would have exactly one solution, and you may not use the same element twice.
//
//  You can return the answer in any order.
//
//  Example 1:
//  Input: nums = [2,7,11,15], target = 9
//  Output: [0,1]
//  Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
//
//  Example 2:
//  Input: nums = [3,2,4], target = 6
//  Output: [1,2]
//
//  Example 3:
//  Input: nums = [3,3], target = 6
//  Output: [0,1]
//
//  Constraints:
//
//  2 <= nums.length <= 104
//  -109 <= nums[i] <= 109
//  -109 <= target <= 109
//  Only one valid answer exists.

def twoSumMut(nums: Array[Int], target: Int): Array[Int] =
  val seen = mutable.HashMap.empty[Int, Int]
  boundary:
    for ((num, i) <- nums.zipWithIndex) do
      seen.get(target - num) match
        case Some(j) => break(Array(j, i))
        case None    => seen(num) = i
    Array.empty

def twoSumFP(nums: Array[Int], target: Int): Array[Int] =
  @tailrec
  def loop(index: Int, seen: HashMap[Int, Int]): Array[Int] =
    val num = nums(index)
    seen.get(target - num) match
      case Some(j) => Array(j, index)
      case None    => loop(index + 1, seen + (num -> index))
  loop(0, HashMap.empty)

def twoSumFold(nums: Array[Int], target: Int): Array[Int] =
  val (_, result) = nums.zipWithIndex.foldLeft(
    (Map.empty[Int, Int], Array.empty[Int])
  ):
    case (acc @ (_, found), _) if found.nonEmpty => acc
    case ((seen, _), (num, i)) =>
      seen.get(target - num) match
        case Some(j) => (seen, Array(j, i))
        case None    => (seen + (num -> i), Array.empty)
  result

@main def twoSum(): Unit =

  val data = List(
    (Array(2, 7, 11, 15) -> 9) -> Array(0,1),
    (Array(3, 2, 4)      -> 6) -> Array(1,2),
    (Array(3, 3)         -> 6) -> Array(0,1)
  )

  val impl = List(twoSumMut, twoSumFP, twoSumFold)

  for
    twoSumDef                  <- impl
    ((nums, target), expected) <- data
  do
    assert:
      twoSumDef(nums, target) sameElements expected
