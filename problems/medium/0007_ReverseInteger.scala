#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec

// 7. Reverse Integer
// Difficulty: Medium
// https://leetcode.com/problems/reverse-integer/

// Given a signed 32-bit integer x, return x with its digits reversed.
// If reversing x causes the value to go outside the signed 32-bit integer range [-231, 231 - 1], then return 0.

// Assume the environment does not allow you to store 64-bit integers (signed or unsigned)

// Example 1:
// Input: x = 123
// Output: 321
// Example 2:

// Example 2:
// Input: x = -123
// Output: -321

// Example 3:
// Input: x = 120
// Output: 21

// Constraints:
// -2^31 <= x <= 2^31 - 1

def reverseIntegerAsString(input: Int): Int =
  val reversed = input.toString.reverse
  val result = if input < 0 then "-" + reversed.dropRight(1) else reversed
  try result.toInt catch case _: NumberFormatException => 0

def reverseIntegerTailrec(input: Int): Int =
  @tailrec
  def loop(x: Int, result: Int): Int =
    if x == 0 then result
    else
      val pop = x % 10
      val res = result * 10 + pop
      if res / 10 != result then return 0
      loop(x / 10, res)

  loop(input, 0)

def reverseIntegerLoop(input: Int): Int =
  var x = input
  var result = 0
  while x != 0 do
    val pop = x % 10
    x /= 10
    if result > Int.MaxValue / 10 || (result == Int.MaxValue / 10 && pop > 7) then return 0
    if result < Int.MinValue / 10 || (result == Int.MinValue / 10 && pop < -8) then return 0
    result = result * 10 + pop
  result

@main def reverseInteger(): Unit =

  val data = List(
    123 -> 321,
    -123 -> -321,
    120 -> 21,
    0 -> 0,
    1534236469 -> 0
  )

  val impl = List(
    reverseIntegerAsString,
    reverseIntegerTailrec,
    reverseIntegerLoop
  )

  for
    reverseIntegerDef <- impl
    (input, expected) <- data
  do
    assert:
      reverseIntegerDef(input) == expected
