#!/usr/bin/env -S scala shebang

// 6. Zigzag Conversion
// Difficulty: Medium
// https://leetcode.com/problems/zigzag-conversion/

// The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)
// P   A   H   N
// A P L S I I G
// Y   I   R
// And then read line by line: "PAHNAPLSIIGYIR"
// Write the code that will take a string and make this conversion given a number of rows:
// string convert(string s, int numRows);

// Example 1:
// Input: s = "PAYPALISHIRING", numRows = 3
// Output: "PAHNAPLSIIGYIR"

// Example 2:
// Input: s = "PAYPALISHIRING", numRows = 4
// Output: "PINALSIGYAHRPI"
// Explanation:
// P     I    N
// A   L S  I G
// Y A   H R
// P     I

// Example 3:
// Input: s = "A", numRows = 1
// Output: "A"

// Constraints:
// 1 <= s.length <= 1000
// s consists of English letters (lower-case and upper-case), ',' and '.'.
// 1 <= numRows <= 1000

def zigzagConversionLazyList(s: String, numRows: Int): String =
  if s.isEmpty || numRows < 1 then
    throw new IllegalArgumentException("1 <= s.length <= 1000 and 1 <= numRows <= 1000")
  if numRows == 1 then
    return s

  val arr = Array.fill(Math.min(numRows, s.length))("")
  val idx = LazyList
    .continually((0 until numRows) ++ (numRows - 2 until 0 by -1))
    .flatten
    .take(s.length)

  s.zip(idx).foreach { case (c, row) => arr(row) += c }
  arr.mkString("")

@main def zigzagConversion(): Unit =

  val data = List(
    ("PAYPALISHIRING", 3) -> "PAHNAPLSIIGYIR",
    ("PAYPALISHIRING", 4) -> "PINALSIGYAHRPI",
    ("A", 1) -> "A"
  )

  val impl = List(
    zigzagConversionLazyList
  )

  for
    zigzagConversionDef        <- impl
    ((str, numRows), expected) <- data
  do
    assert:
      zigzagConversionDef(str, numRows) == expected
