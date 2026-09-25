#!/usr/bin/env -S scala shebang

// 9. Palindrome Number
// Difficulty: Easy
// https://leetcode.com/problems/palindrome-number/

// Given an integer x, return true if x is a palindrome, and false otherwise.

// Example 1:
// Input: x = 121
// Output: true
// Explanation: 121 reads as 121 from left to right and from right to left.

// Example 2:
// Input: x = -121
// Output: false
// Explanation: From left to right, it reads -121. From right to left, it becomes 121-.
// Therefore it is not a palindrome.

// Example 3:
// Input: x = 10
// Output: false
// Explanation: Reads 01 from right to left. Therefore it is not a palindrome.

// Constraints:
// -231 <= x <= 231 - 1

// Follow up: Could you solve it without converting the integer to a string?

import scala.annotation.tailrec

def palindromeNumberHalfReverse(x: Int): Boolean =
  if x < 0 || (x % 10 == 0 && x != 0) then false
  else
    var rem = x
    var rev = 0
    while rem > rev do
      rev = rev * 10 + rem % 10
      rem /= 10
    rem == rev || rem == rev / 10

def palindromeNumberFullReverse(x: Int): Boolean =
  if x < 0 then false
  else
    var rem = x
    var rev = 0L
    while rem > 0 do
      rev = rev * 10 + rem % 10
      rem /= 10
    rev == x

def palindromeNumberCompareDigits(x: Int): Boolean =
  if x < 0 then false
  else
    var rem = x
    var divisor = 1
    while rem / divisor >= 10 do divisor *= 10
    while rem > 0 do
      if rem / divisor != rem % 10 then return false
      rem = (rem % divisor) / 10
      divisor /= 100
    true

def palindromeNumberTailrec(x: Int): Boolean =
  if x < 0 || (x % 10 == 0 && x != 0) then false
  else
    @tailrec
    def loop(remaining: Int, reversedHalf: Int): Boolean =
      if remaining <= reversedHalf then
        remaining == reversedHalf || remaining == reversedHalf / 10
      else loop(remaining / 10, reversedHalf * 10 + remaining % 10)

    loop(x, 0)

def palindromeNumberFold(x: Int): Boolean =
  if x < 0 then false
  else
    val digits = List.unfold(x): remaining =>
      if remaining == 0 then None
      else Some((remaining % 10, remaining / 10))
    digits.foldLeft(List.empty[Int])((reversed, digit) => digit :: reversed) == digits


@main def palindromeNumber(): Unit =

  val data = List(
    121  -> true,
    -121 -> false,
    10   -> false,
    0    -> true,
    1221 -> true,
    1231 -> false,
    12321 -> true,
    2147447412 -> true,
    2147483647 -> false
  )

  val impl = List(
    palindromeNumberHalfReverse,
    palindromeNumberFullReverse,
    palindromeNumberCompareDigits,
    palindromeNumberTailrec,
    palindromeNumberFold
  )

  for
    palindromeNumberDef <- impl
    (input, expected) <- data
  do
    assert:
      palindromeNumberDef(input) == expected
