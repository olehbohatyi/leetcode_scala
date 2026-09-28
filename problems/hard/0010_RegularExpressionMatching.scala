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

def regularExpressionMatchingSolution(s: String, p: String): Boolean =
  if p.isEmpty then s.isEmpty
  else if p.length == 1 && p(0) == '*' then true
  else if p.length == 1 && p(0) != '*' then s == p
  else if p.length >= 2 && p(1) == '*' then
    val firstMatch = s.nonEmpty && (s(0) == p(0) || p(0) == '.')
    regularExpressionMatchingSolution(s, p.drop(2)) ||
      (firstMatch && regularExpressionMatchingSolution(s.drop(1), p))
  else
    s.nonEmpty && (s(0) == p(0) || p(0) == '.') &&
      regularExpressionMatchingSolution(s.drop(1), p.drop(1))

@main def regularExpressionMatching(): Unit =

  val data = List(
    ("aa", "a")  -> false,
    ("aa", "a*") -> true,
    ("ab", ".*") -> true,
    ("a", "b*")  -> false
  )

  val impl = List(
    regularExpressionMatchingSolution
  )

  for
    regularExpressionMatchingDef <- impl
    ((s, p), expected) <- data
  do
    assert:
      regularExpressionMatchingDef(s, p) == expected
