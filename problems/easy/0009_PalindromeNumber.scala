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

def palindromeNumberHalfReverse(x: Int): Boolean =
  if x < 0 || (x % 10 == 0 && x != 0) then false
  else
    var rem = x
    var rev = 0
    while rem > rev do
      rev = rev * 10 + rem % 10
      rem /= 10
    rem == rev || rem == rev / 10

@main def palindromeNumber(): Unit =

  val data = List(
    121  -> true,
    -121 -> false,
    10   -> false,
    0    -> true,
    1221 -> true,
    1231 -> false
  )

  val impl = List(
    palindromeNumberHalfReverse
  )

  for
    palindromeNumberDef <- impl
    (input, expected) <- data
  do
    assert:
      palindromeNumberDef(input) == expected
