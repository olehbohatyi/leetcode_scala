#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

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

def longestCommonPrefixFold(strs: Array[String]): String =
  if strs.isEmpty then ""
  else
    val srt = strs.sortBy(_.length)
    if srt.head.isEmpty then ""
    else
      srt.tail.foldLeft(srt.head):
        case (m, w) if m.isEmpty        => m
        case (m, w) if !w.startsWith(m) =>
          var min = m
          while !w.startsWith(min) do min = min.substring(0, min.length - 1)
          min
        case (m, _) => m

def longestCommonPrefixIndex(strs: Array[String]): String =
  if strs.isEmpty then ""
  else
    val srt = strs.sortBy(_.length)
    var min = srt.head
    if min.isEmpty then ""
    else
      boundary:
        for str <- srt.tail do
          if min(0) != str(0) then break("")
          while min != str.substring(0, min.length) do min = min.substring(0, min.length - 1)
        min

def longestCommonPrefixTailrec(strs: Array[String]): String =
  if strs.isEmpty then ""
  else
    val srt = strs.sortBy(_.length)
    val min = srt.head

    @tailrec
    def loop(pre: String, rem: List[String]): String =
      if pre.isEmpty then ""
      else
        rem match
          case Nil          => pre
          case head :: tail =>
            if head.substring(0, pre.length) == pre then loop(pre, tail)
            else loop(pre.substring(0, pre.length - 1), rem)

    loop(min, srt.tail.toList)

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
    longestCommonPrefixForZip,
    longestCommonPrefixFold,
    longestCommonPrefixIndex,
    longestCommonPrefixTailrec
  )

  for
    longestCommonPrefixDef <- impl
    (input, check)         <- data
  do
    assert:
      longestCommonPrefixDef(input) == check
