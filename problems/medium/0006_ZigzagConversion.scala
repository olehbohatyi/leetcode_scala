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

import scala.annotation.tailrec

private def requireValid(s: String, numRows: Int): Unit =
  if s.isEmpty || numRows < 1 then
    throw new IllegalArgumentException("1 <= s.length <= 1000 and 1 <= numRows <= 1000")

def zigzagConversionLazyList(s: String, numRows: Int): String =
  requireValid(s, numRows)
  if numRows == 1 then
    return s

  val rows = Array.fill(Math.min(numRows, s.length))(StringBuilder())
  val idx = Iterator
    .continually((0 until numRows) ++ (numRows - 2 until 0 by -1))
    .flatten

  s.zip(idx).foreach: (c, row) =>
    rows(row).append(c)
  rows.mkString

def zigzagConversionIndexes(s: String, numRows: Int): String =
  requireValid(s, numRows)
  if numRows == 1 then
    return s

  val cycle = 2 * numRows - 2
  val sb = StringBuilder()
  for
    row <- 0 until Math.min(numRows, s.length)
    i   <- row until s.length by cycle
  do
    sb.append(s(i))
    val diag = i + cycle - 2 * row
    if row > 0 && row < numRows - 1 && diag < s.length then
      sb.append(s(diag))
  sb.toString

def zigzagConversionTailrec(s: String, numRows: Int): String =
  requireValid(s, numRows)
  if numRows == 1 then
    return s

  @tailrec
  def loop(i: Int, row: Int, step: Int, rows: Vector[String]): Vector[String] =
    if i == s.length then rows
    else
      val nextStep =
        if row == 0 then 1
        else if row == numRows - 1 then -1
        else step
      loop(i + 1, row + nextStep, nextStep, rows.updated(row, rows(row) + s(i)))

  loop(0, 0, 1, Vector.fill(numRows)("")).mkString

@main def zigzagConversion(): Unit =

  val data = List(
    ("PAYPALISHIRING", 3) -> "PAHNAPLSIIGYIR",
    ("PAYPALISHIRING", 4) -> "PINALSIGYAHRPI",
    ("A", 1)              -> "A",
    ("AB", 5)             -> "AB",
    ("ABCD", 2)           -> "ACBD",
    ("A,B.", 3)           -> "A,.B"
  )

  val impl = List(
    zigzagConversionLazyList,
    zigzagConversionIndexes,
    zigzagConversionTailrec
  )

  for
    zigzagConversionDef        <- impl
    ((str, numRows), expected) <- data
  do
    assert:
      zigzagConversionDef(str, numRows) == expected
