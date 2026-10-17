#!/usr/bin/env -S scala shebang

// 13. Roman to Integer
// Difficulty: Easy
// https://leetcode.com/problems/roman-to-integer/

// Convert a valid Roman numeral to an integer. Symbols are ordered from largest
// to smallest except for the six standard subtractive pairs: IV, IX, XL, XC,
// CD, and CM.

// Example 1:
// Input: s = "III"
// Output: 3
//
// Example 2:
// Input: s = "LVIII"
// Output: 58
//
// Example 3:
// Input: s = "MCMXCIV"
// Output: 1994

// Constraints:
// 1 <= s.length <= 15; s is valid and represents a value from 1 to 3999.

private val romanValue: Map[Char, Int] = Map(
  'I' -> 1,
  'V' -> 5,
  'X' -> 10,
  'L' -> 50,
  'C' -> 100,
  'D' -> 500,
  'M' -> 1000
)

def romanToIntegerRightToLeft(s: String): Int =
  var total = 0
  var prevs = 0
  var index = s.length - 1

  while index >= 0 do
    val crrnt = romanValue(s(index))
    if crrnt < prevs then total -= crrnt
    else total += crrnt
    prevs = crrnt
    index -= 1

  total

def romanToIntegerLeftToRight(s: String): Int =
  var total = 0
  var index = 0

  while index < s.length do
    val crrnt = romanValue(s(index))
    if index + 1 < s.length then
      val next = romanValue(s(index + 1))
      if crrnt < next then
        total += (next - crrnt)
        index += 2
      else
        total += crrnt
        index += 1
    else
      total += crrnt
      index += 1

  total

@main def romanToInteger(): Unit =

  val data = List(
    "III"        -> 3,
    "LVIII"      -> 58,
    "MCMXCIV"    -> 1994,
    "IV"         -> 4,
    "IX"         -> 9,
    "XL"         -> 40,
    "XC"         -> 90,
    "CD"         -> 400,
    "CM"         -> 900,
    "MMMCMXCIX"  -> 3999
  )

  val impl = List(
    romanToIntegerRightToLeft,
    romanToIntegerLeftToRight
  )

  for
    romanToIntegerDef <- impl
    (input, expected) <- data
  do
    assert:
      romanToIntegerDef(input) == expected