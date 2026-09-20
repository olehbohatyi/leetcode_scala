#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec
import scala.collection.mutable

// 3. Longest Substring Without Repeating Characters

// Given a string s, find the length of the longest substringwithout duplicate characters.

// Example 1:
// Input: s = "abcabcbb"
// Output: 3
// Explanation: The answer is "abc", with the length of 3. Note that "bca" and "cab" are also correct answers.

// Example 2:
// Input: s = "bbbbb"
// Output: 1
// Explanation: The answer is "b", with the length of 1.

// Example 3:
// Input: s = "pwwkew"
// Output: 3
// Explanation: The answer is "wke", with the length of 3.
// Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.

// Constraints:
// 0 <= s.length <= 105
// s consists of English letters, digits, symbols and spaces.

def longestSubstringWithoutRepeatingCharactersMut(s: String): Int =
  val seen = mutable.HashMap.empty[Char, Int]
  var start = 0
  var best = 0

  for (i <- s.indices) do
    val chr = s(i)
    seen.get(chr) match
      case Some(prev) if prev >= start =>
        start = prev + 1
      case _ =>
    best = math.max(best, i - start + 1)
    seen(chr) = i

  best

def longestSubstringWithoutRepeatingCharactersFold(input: String): Int = {
  input.zipWithIndex.foldLeft((Map.empty[Char, Int], 0, 0)):
    case ((map, start, best), (chr, idx)) =>
      val newStart = map.get(chr) match {
        case Some(prev) if prev >= start => prev + 1
        case _ => start
      }
      val newBest = math.max(best, idx - newStart + 1)
      (map.updated(chr, idx), newStart, newBest)
  ._3
}

def longestSubstringWithoutRepeatingCharactersFP(s: String): Int =
  @tailrec
  def loop(index: Int, start: Int, best: Int, lastSeen: Map[Char, Int]): Int =
    if index == s.length then best
    else
      val chr = s(index)
      val newStart =
        lastSeen.get(chr) match
          case Some(previous) if previous >= start => previous + 1
          case _                                   => start

      val newBest = math.max(best, index - newStart + 1)

      loop(index + 1, newStart, newBest, lastSeen.updated(chr, index))

  loop(0, 0, 0, Map.empty)

@main def longestSubstringWithoutRepeatingCharacters(): Unit =

  val data = List(
    "abcabcbb" -> 3,
    "bbbbb"    -> 1,
    "pwwkew"   -> 3
  )

  val impl = List(
    longestSubstringWithoutRepeatingCharactersMut,
    longestSubstringWithoutRepeatingCharactersFold,
    longestSubstringWithoutRepeatingCharactersFP
  )

  for
    longestSubstringWithoutRepeatingCharactersDef <- impl
    (str, expected)                               <- data
  do
    assert:
      longestSubstringWithoutRepeatingCharactersDef(str) == expected  
