#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec
import scala.collection.mutable

// 5. Longest Palindromic Substring

// Given a string s, return the longest palindromic substring in s.

// Example 1:
// Input: s = "babad"
// Output: "bab"
// Explanation: "aba" is also a valid answer.

// Example 2:
// Input: s = "cbbd"
// Output: "bb"

// Constraints:
// 1 <= s.length <= 1000
// s consist of only digits and English letters.

def longestPalindromicSubstringLoop(s: String): String =
  if s.isEmpty then ""
  else
    var longest = ""
    for i <- s.indices do
      for j <- i until s.length do
        val substring = s.substring(i, j + 1)
        if substring == substring.reverse && substring.length > longest.length then
          longest = substring
    longest

def longestPalindromicSubstringMut(s: String): String =
  def expandAroundCenter(s: String, left: Int, right: Int): Int =
      var L = left
      var R = right
      while L >= 0 && R < s.length && s(L) == s(R) do
        L -= 1
        R += 1
      R - L - 1 

  if s.isEmpty then ""
  else
    var start = 0
    var end   = 0
    for i <- s.indices do
      val len1 = expandAroundCenter(s, i, i)
      val len2 = expandAroundCenter(s, i, i + 1)
      val len  = math.max(len1, len2)
      if len > end - start then
        start = i - (len - 1) / 2
        end   = i + len / 2

    s.substring(start, end + 1)

def longestPalindromicSubstringFP(s: String): String =
  def expandAroundCenter(s: String, left: Int, right: Int): Int =
    @tailrec
    def loop(L: Int, R: Int): Int =
      if L >= 0 && R < s.length && s(L) == s(R) then
        loop(L - 1, R + 1)
      else
        R - L - 1
    loop(left, right)

  if s.isEmpty then ""
  else
    val (start, end) = s.indices.foldLeft((0, 0)):
      case ((start, end), i) =>
        val len1 = expandAroundCenter(s, i, i)
        val len2 = expandAroundCenter(s, i, i + 1)
        val len  = math.max(len1, len2)
        if len > end - start then
          (i - (len - 1) / 2, i + len / 2)
        else
          (start, end)

    s.substring(start, end + 1)

def longestPalindromicSubstringIndexes(s: String): String =
  def isPal(i: Int, j: Int): Boolean =
    var left = i
    var right = j
    while left < right && s(left) == s(right) do
      left += 1
      right -= 1
    left >= right

  if s.length <= 1 then return s
  else
    var map = mutable.Map.empty[Char, List[Int]]
    for i <- s.indices do
      map.update(s(i), i :: map.getOrElse(s(i), List.empty))
    val (one, two) = map.partition(_._2.length <= 1)
    if (two.isEmpty) then
      return s.head.toString
    else
      var start  = 0
      var end    = 0
      var length = 0
      for indexes <- two.values do
        if indexes.length == 2 &&
          indexes.head - indexes(1) == 1 &&
          length < 2 then
          start = indexes(1)
          end = indexes.head
          length = indexes.length
        else
          for from <- indexes.tail do
            for last <- indexes do
              if last - from <= 1 then
                if last - from + 1 > length then
                  start = from
                  end = last
                  length = last - from + 1
              else
                if isPal(from, last) && last - from + 1 > length then
                  start = from
                  end = last
                  length = last - from + 1
      return s.substring(start, end + 1)

@main def longestPalindromicSubstring(): Unit =

  val data = List(
    "babad"  -> Set("bab", "aba"),
    "baabad" -> Set("baab"),
    "cbbd"   -> Set("bb"),
    "a"      -> Set("a"),
    "ac"     -> Set("a", "c"),
    "abcde"  -> Set("a", "b", "c", "d", "e")
  )

  val impl = List(
      longestPalindromicSubstringLoop,
      longestPalindromicSubstringMut,
      longestPalindromicSubstringFP,
      longestPalindromicSubstringIndexes
    )

  for
    longestPalindromicSubstringDef <- impl
    (input, expected)              <- data
  do
    assert:
      expected.contains(longestPalindromicSubstringDef(input))
