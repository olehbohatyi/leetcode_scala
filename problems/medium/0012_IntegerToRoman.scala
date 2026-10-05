#!/usr/bin/env -S scala shebang

// 12. Integer to Roman
// Difficulty: Medium
// https://leetcode.com/problems/integer-to-roman/

// Roman numerals are formed by appending conversions of decimal place values
// from highest to lowest. The conversions for each place value are:
// 1000: M
// 900: CM
// 500: D
// 400: CD
// 100: C
// 90: XC
// 50: L
// 40: XL
// 10: X
// 9: IX
// 5: V
// 4: IV
// 1: I
//
// Given an integer, convert it to a Roman numeral.

// Example 1:
// Input: num = 3749
// Output: "MMMDCCXLIX"
// Explanation: 3000 = MMM, 700 = DCC, 40 = XL, and 9 = IX.

// Example 2:
// Input: num = 58
// Output: "LVIII"
// Explanation: 50 = L, 5 = V, and 3 = III.

// Example 3:
// Input: num = 1994
// Output: "MCMXCIV"
// Explanation: 1000 = M, 900 = CM, 90 = XC, and 4 = IV.

// Constraints:
// 1 <= num <= 3999

def integerToRomanApproach(num: Int): String = ???

@main def integerToRoman(): Unit =

  val data = List(
    3749 -> "MMMDCCXLIX",
    58   -> "LVIII",
    1994 -> "MCMXCIV"
  )

  val impl = List(
    integerToRomanApproach
  )

  for
    integerToRomanDef <- impl
    (num, expected)   <- data
  do
    assert:
      integerToRomanDef(num) == expected
