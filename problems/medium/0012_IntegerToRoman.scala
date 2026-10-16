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

// Roman numerals are formed by appending the conversions of decimal place values from highest
// to lowest. Converting a decimal place value into a Roman numeral has the following rules:

// If the value does not start with 4 or 9, select the symbol of the maximal value that can be
// subtracted from the input, append that symbol to the result, subtract its value, and convert
// the remainder to a Roman numeral.

// If the value starts with 4 or 9 use the subtractive form representing one symbol subtracted
// from the following symbol, for example, 4 is 1 (I) less than 5 (V): IV and 9 is 1 (I) less
// than 10 (X): IX. Only the following subtractive forms are used: 4 (IV), 9 (IX), 40 (XL),
// 90 (XC), 400 (CD) and 900 (CM).

// Only powers of 10 (I, X, C, M) can be appended consecutively at most 3 times to represent
// multiples of 10. You cannot append 5 (V), 50 (L), or 500 (D) multiple times. If you need to
// append a symbol 4 times use the subtractive form.

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

val romanNumerals = List(
  1000 -> "M",
  900  -> "CM",
  500  -> "D",
  400  -> "CD",
  100  -> "C",
  90   -> "XC",
  50   -> "L",
  40   -> "XL",
  10   -> "X",
  9    -> "IX",
  5    -> "V",
  4    -> "IV",
  1    -> "I"
)

def integerToRomanWhile(num: Int): String =
  var n = num
  val sb = new StringBuilder

  for (value, numeral) <- romanNumerals do
    while n >= value do
      sb.append(numeral)
      n -= value

  sb.toString()

def integerToRomanRec(num: Int): String =
  if num == 0 then ""
  else
    val (value, numeral) = romanNumerals.find((value, _) => num >= value).get
    numeral + integerToRomanRec(num - value)

@main def integerToRoman(): Unit =

  val data = List(
    3749 -> "MMMDCCXLIX",
    58   -> "LVIII",
    1994 -> "MCMXCIV",
    888  -> "DCCCLXXXVIII",
    999  -> "CMXCIX",
    1    -> "I",
    3999 -> "MMMCMXCIX"
  )

  val impl = List(
    integerToRomanWhile,
    integerToRomanRec
  )

  for
    integerToRomanDef <- impl
    (num, expected)   <- data
  do
    assert:
      integerToRomanDef(num) == expected
