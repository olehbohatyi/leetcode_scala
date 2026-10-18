#!/usr/bin/env -S scala shebang

// 14. Longest Common Prefix
// Difficulty: Easy
// https://leetcode.com/problems/longest-common-prefix/

// Write a function to find the longest common prefix string amongst an array of
// strings. If there is no common prefix, return an empty string.

// Example 1:
// Input: strs = ["flower", "flow", "flight"]
// Output: "fl"

// Example 2:
// Input: strs = ["dog", "racecar", "car"]
// Output: ""

// Constraints:
// 1 <= strs.length <= 200
// 0 <= strs[i].length <= 200
// strs[i] consists of lowercase English letters if non-empty.

def longestCommonPrefixForWhile(strs: Array[String]): String =
  if strs.isEmpty then ""
  else
    val srt = strs.sortBy(_.length)
    var min = srt.head
    for str <- srt.tail do while !str.startsWith(min) do min = min.substring(0, min.length - 1)
    min

def longestCommonPrefixForZip(strs: Array[String]): String =
  if strs.isEmpty then ""
  else
    val srt = strs.sortBy(_.length)
    val min = srt.head
    min.zipWithIndex.takeWhile((c, i) => srt.tail.forall(_.charAt(i) == c)).map(_._1).mkString

@main def longestCommonPrefix(): Unit =

  val data = List(
    Array("flower", "flow", "flight")              -> "fl",
    Array("dog", "racecar", "car")                 -> "",
    Array("apple", "app", "april")                 -> "ap",
    Array("interview", "intermediate", "internal") -> "inter",
    Array("prefix", "prefixes", "prefixed")        -> "prefix",
    Array("same", "same", "same")                  -> "same",
    Array("a", "b", "c")                           -> "",
    Array("a")                                     -> "a",
    Array.empty[String]                            -> "",
    Array("", "str", "string")                     -> "",
    Array("str", "", "string")                     -> "",
    Array("reflower", "flow", "flight")            -> ""
  )

  val impl = List(
    longestCommonPrefixForWhile,
    longestCommonPrefixForZip
  )

  for
    longestCommonPrefixDef <- impl
    (input, check)         <- data
  do
    assert:
      longestCommonPrefixDef(input) == check
