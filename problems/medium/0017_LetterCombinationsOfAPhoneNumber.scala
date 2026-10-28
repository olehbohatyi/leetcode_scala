#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec

// 17. Letter Combinations of a Phone Number
// Difficulty: Medium
// https://leetcode.com/problems/letter-combinations-of-a-phone-number/

// Given a string containing digits from 2-9 inclusive, return all possible letter
// combinations that the number could represent. Return the answer in any order.

// A mapping of digits to letters (just like on the telephone buttons) is given below.
// Note that 1 does not map to any letters.

// Example 1:
// Input: digits = "23"
// Output: ["ad","ae","af","bd","be","bf","cd","ce","cf"]

// Example 2:
// Input: digits = "2"
// Output: ["a","b","c"]

// Constraints:
// 1 <= digits.length <= 4
// digits[i] is a digit in the range ['2', '9'].

private val phoneNumberLetters = Map(
  '2' -> List("a", "b", "c"),
  '3' -> List("d", "e", "f"),
  '4' -> List("g", "h", "i"),
  '5' -> List("j", "k", "l"),
  '6' -> List("m", "n", "o"),
  '7' -> List("p", "q", "r", "s"),
  '8' -> List("t", "u", "v"),
  '9' -> List("w", "x", "y", "z")
)

def letterCombinationsOfAPhoneNumberTailrec(input: String): List[String] =
  @tailrec
  def loop(chars: List[Char], combs: List[String]): List[String] =
    chars match
      case Nil                      => combs
      case digit :: remainingDigits =>
        val next = combs.flatMap: comb =>
          phoneNumberLetters(digit).map(comb + _)
        loop(remainingDigits, next)

  if input.isEmpty then List.empty[String]
  else loop(input.toList, List(""))

def letterCombinationsOfAPhoneNumberBacktracking(input: String): List[String] =
  val combinations = scala.collection.mutable.ListBuffer[String]()

  def build(index: Int, prefix: String): Unit =
    if index == input.length then combinations += prefix
    else
      phoneNumberLetters(input(index)).foreach: letter =>
        build(index + 1, prefix + letter)

  if input.nonEmpty then build(0, "")
  combinations.toList

def letterCombinationsOfAPhoneNumberRecursive(input: String): List[String] =
  def build(index: Int): List[String] =
    if index == input.length then List("")
    else
      for
        letter <- phoneNumberLetters(input(index))
        suffix <- build(index + 1)
      yield letter + suffix

  if input.isEmpty then List.empty[String]
  else build(0)

def letterCombinationsOfAPhoneNumberFold(input: String): List[String] =
  if input.isEmpty then List.empty[String]
  else
    input.foldLeft(List("")): (combinations, digit) =>
      combinations.flatMap: combination =>
        phoneNumberLetters(digit).map(combination + _)

@main def letterCombinationsOfAPhoneNumber(): Unit =

  val data = List(
    ""    -> List.empty[String],
    "23"  -> List("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"),
    "2"   -> List("a", "b", "c"),
    "79"  -> List("pw", "px", "py", "pz", "qw", "qx", "qy", "qz", "rw", "rx", "ry", "rz", "sw", "sx", "sy", "sz"),
    "234" -> List(
      "adg",
      "adh",
      "adi",
      "aeg",
      "aeh",
      "aei",
      "afg",
      "afh",
      "afi",
      "bdg",
      "bdh",
      "bdi",
      "beg",
      "beh",
      "bei",
      "bfg",
      "bfh",
      "bfi",
      "cdg",
      "cdh",
      "cdi",
      "ceg",
      "ceh",
      "cei",
      "cfg",
      "cfh",
      "cfi"
    )
  )

  val impl = List(
    letterCombinationsOfAPhoneNumberTailrec,
    letterCombinationsOfAPhoneNumberBacktracking,
    letterCombinationsOfAPhoneNumberRecursive,
    letterCombinationsOfAPhoneNumberFold
  )

  for
    letterCombinationsOfAPhoneNumberDef <- impl
    (input, check)                      <- data
  do
    assert:
      letterCombinationsOfAPhoneNumberDef(input) == check
