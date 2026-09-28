#!/usr/bin/env -S scala shebang

// 10. Regular Expression Matching
// Difficulty: Hard
// https://leetcode.com/problems/regular-expression-matching/

// Given an input string s and a pattern p, implement regular expression
// matching with support for '.' and '*', where:
// - '.' Matches any single character.
// - '*' Matches zero or more of the preceding element.
//
// Return a boolean indicating whether the matching covers the entire input string (not partial).

// Example 1:
// Input: s = "aa", p = "a"
// Output: false
// Explanation: "a" does not match the entire string "aa".

// Example 2:
// Input: s = "aa", p = "a*"
// Output: true
// Explanation: '*' means zero or more of the preceding element, 'a'.
// Therefore, by repeating 'a' once, it becomes "aa".

// Example 3:
// Input: s = "ab", p = ".*"
// Output: true
// Explanation: ".*" means "zero or more (*) of any character (.)".

// Constraints:
// 1 <= s.length <= 20
// 1 <= p.length <= 30
// s contains only lowercase English letters.
// p contains only lowercase English letters, '.', and '*'.
// It is guaranteed for each appearance of the character '*', there will be a
// previous valid character to match.

import scala.annotation.tailrec

def regexCharactersMatch(patternChar: Char, inputChar: Char): Boolean =
  patternChar == '.' || patternChar == inputChar

def regularExpressionMatchingRecursive(s: String, p: String): Boolean =
  if p.isEmpty then s.isEmpty
  else if p.length >= 2 && p(1) == '*' then
    val firstMatch = s.nonEmpty && regexCharactersMatch(p(0), s(0))
    regularExpressionMatchingRecursive(s, p.drop(2)) ||
      (firstMatch && regularExpressionMatchingRecursive(s.drop(1), p))
  else
    s.nonEmpty && regexCharactersMatch(p(0), s(0)) &&
      regularExpressionMatchingRecursive(s.drop(1), p.drop(1))

def regularExpressionMatchingTailrec(s: String, p: String): Boolean =
  val columns = p.length + 1
  val table = Array.fill((s.length + 1) * columns)(false)
  table(0) = true

  @tailrec
  def fill(index: Int): Boolean =
    if index == table.length then table(index - 1)
    else
      val inputLength = index / columns
      val patternLength = index % columns
      if patternLength > 0 then
        val patternChar = p(patternLength - 1)
        if patternChar == '*' then
          if patternLength >= 2 then
            val repeatedChar = p(patternLength - 2)
            table(index) = table(index - 2) ||
              (inputLength > 0 && regexCharactersMatch(repeatedChar, s(inputLength - 1)) &&
                table(index - columns))
        else if inputLength > 0 then
          table(index) = regexCharactersMatch(patternChar, s(inputLength - 1)) &&
            table(index - columns - 1)
      fill(index + 1)

  fill(1)

def regularExpressionMatchingFold(s: String, p: String): Boolean =
  val initialRow = Array.fill(p.length + 1)(false)
  initialRow(0) = true
  val emptyInputRow = p.indices.foldLeft(initialRow): (row, patternIndex) =>
    if p(patternIndex) == '*' then row(patternIndex + 1) = row(patternIndex - 1)
    row

  val finalRow = s.indices.foldLeft(emptyInputRow): (previousRow, inputIndex) =>
    p.indices.foldLeft(Array.fill(p.length + 1)(false)): (currentRow, patternIndex) =>
      val patternLength = patternIndex + 1
      val patternChar = p(patternIndex)
      if patternChar == '*' then
        val repeatedChar = p(patternIndex - 1)
        currentRow(patternLength) = currentRow(patternLength - 2) ||
          (regexCharactersMatch(repeatedChar, s(inputIndex)) && previousRow(patternLength))
      else
        currentRow(patternLength) = regexCharactersMatch(patternChar, s(inputIndex)) &&
          previousRow(patternLength - 1)
      currentRow

  finalRow(p.length)

@main def regularExpressionMatching(): Unit =

  val data = List(
    ("aa", "a")  -> false,
    ("aa", "a*") -> true,
    ("ab", ".*") -> true,
    ("a", "b*")  -> false,
    ("a", ".") -> true,
    ("aab", "c*a*b") -> true,
    ("mississippi", "mis*is*p*.") -> false,
    ("", "a*b*") -> true,
    ("", ".") -> false
  )

  val impl = List(
    regularExpressionMatchingRecursive,
    regularExpressionMatchingTailrec,
    regularExpressionMatchingFold
  )

  for
    regularExpressionMatchingDef <- impl
    ((s, p), expected) <- data
  do
    assert:
      regularExpressionMatchingDef(s, p) == expected
