#!/usr/bin/env -S scala shebang

// 8. String to Integer (atoi)
// Difficulty: Medium
// https://leetcode.com/problems/string-to-integer-atoi/

// Implement the myAtoi(string s) function,
// which converts a string to a 32-bit signed integer.

// The algorithm for myAtoi(string s) is as follows:

// 1. Whitespace: Ignore any leading whitespace (" ").
// 2. Signedness: Determine the sign by checking if the next character is '-' or '+', assuming positivity if neither present.
// 3. Conversion: Read the integer by skipping leading zeros until a non-digit character is encountered or the end of the string is reached. If no digits were read, then the result is 0.
// 4. Rounding: If the integer is out of the 32-bit signed integer range [-231, 231 - 1], then round the integer to remain in the range. Specifically, integers less than -231 should be rounded to -231, and integers greater than 231 - 1 should be rounded to 231 - 1.

// Return the integer as the final result.

// Example 1:
// Input: s = "42"
// Output: 42

// Explanation:
// The underlined characters are what is read in and the caret is the current reader position.
// Step 1: "42" (no characters read because there is no leading whitespace)
// Step 2: "42" (no characters read because there is neither a '-' nor '+')
// Step 3: "42" ("42" is read in)

// Example 2:
// Input: s = " -042"
// Output: -42

// Explanation:
// Step 1: "   -042" (leading whitespace is read and ignored)
// Step 2: "   -042" ('-' is read, so the result should be negative)
// Step 3: "   -042" ("042" is read in, leading zeros ignored in the result)

// Example 3:
// Input: s = "1337c0d3"
// Output: 1337

// Explanation:

// Step 1: "1337c0d3" (no characters read because there is no leading whitespace)
// Step 2: "1337c0d3" (no characters read because there is neither a '-' nor '+')
// Step 3: "1337c0d3" ("1337" is read in; reading stops because the next character is a non-digit)

// Example 4:
// Input: s = "0-1"
// Output: 0

// Explanation:
// Step 1: "0-1" (no characters read because there is no leading whitespace)
// Step 2: "0-1" (no characters read because there is neither a '-' nor '+')
// Step 3: "0-1" ("0" is read in; reading stops because the next character is a non-digit)

// Example 5:
// Input: s = "words and 987"
// Output: 0

// Explanation:
// Reading stops at the first non-digit character 'w'.

// Constraints:
// 0 <= s.length <= 200
// s consists of English letters (lower-case and upper-case), digits (0-9), ' ', '+', '-', and '.'.

def stringToIntegerAtoiWhile(str: String): Int =

  if str.isEmpty then return 0

  var idx = 0
  var res = 0
  var pos = true

  while idx < str.length && str(idx) == ' ' do idx += 1
  
  if idx < str.length && str(idx) == '-' then
    idx += 1
    pos = false

  if idx < str.length && str(idx) == '+' && pos then
    idx += 1

  val max = Int.MaxValue / 10
  val rem = Int.MaxValue % 10

  while idx < str.length && str(idx) >= '0' && str(idx) <= '9' do
    if res > max || (res == max && (str(idx) - '0') > rem) then
      return if pos then Int.MaxValue else Int.MinValue
    res = res * 10 + (str(idx) - '0')
    idx += 1

  res * (if pos then 1 else -1)

def stringToIntegerAtoiFold(str: String): Int =

  if str.isEmpty then return 0

  val (res, pos) = str.foldLeft((0, true, false)) { case ((res, pos, started), c) =>
    if !started && c == ' ' then (res, pos, false)
    else if !started && c == '-' then (res, false, true)
    else if !started && c == '+' then (res, true, true)
    else if c >= '0' && c <= '9' then
      val digit = c - '0'
      val max = Int.MaxValue / 10
      val rem = Int.MaxValue % 10
      if res > max || (res == max && digit > rem) then
        return if pos then Int.MaxValue else Int.MinValue
      (res * 10 + digit, pos, true)
    else if started then return res * (if pos then 1 else -1)
    else return 0
  } match {
    case (res, pos, _) => (res, pos)
  }

  res * (if pos then 1 else -1)

@main def stringToIntegerAtoi(): Unit =

  val data = List(
    "   -042" -> -42,
    "42" -> 42,
    "1337c0d3" -> 1337,
    "0-1" -> 0,
    "words and 987" -> 0,
    "" -> 0,
    "-" -> 0,
    "+" -> 0,
    "   " -> 0,
    "2147483648" -> 2147483647,
  )

  val impl = List(
    stringToIntegerAtoiWhile,
    stringToIntegerAtoiFold
  )

  for
    stringToIntegerDef <- impl
    (input, expected)  <- data
  do
    assert:
      stringToIntegerDef(input) == expected
