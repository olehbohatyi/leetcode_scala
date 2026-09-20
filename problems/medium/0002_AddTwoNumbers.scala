#!/usr/bin/env -S scala shebang

import scala.annotation.tailrec

// 2. Add Two Numbers
// Difficulty: Medium
// https://leetcode.com/problems/add-two-numbers/

// You are given two non-empty linked lists representing two non-negative integers.
// The digits are stored in reverse order, and each of their nodes contains a single digit.
// Add the two numbers and return the sum as a linked list.
// You may assume the two numbers do not contain any leading zero, except the number 0 itself.

// Example 1:
// Input: l1 = [2,4,3], l2 = [5,6,4]
// Output: [7,0,8]
// Explanation: 342 + 465 = 807.

// Example 2:
// Input: l1 = [0], l2 = [0]
// Output: [0]

// Example 3:
// Input: l1 = [9,9,9,9,9,9,9], l2 = [9,9,9,9]
// Output: [8,9,9,9,0,0,0,1]

// Constraints:
// The number of nodes in each linked list is in the range [1, 100].
// 0 <= Node.val <= 9
// It is guaranteed that the list represents a number that does not have leading zeros.

/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
class ListNode(_x: Int = 0, _next: ListNode = null):
  var next: ListNode = _next
  var x: Int = _x

  override def toString: String = s"ListNode($x, ${Option(next).map(_.toString).getOrElse("")})"

  override def equals(obj: Any): Boolean = obj match
    case that: ListNode => this.x == that.x && this.next == that.next
    case _ => false

  override def hashCode(): Int =
    import scala.util.hashing.MurmurHash3
    // BAD: Non-value field is accessed in 'hashCode()'
    val nextHash = if next == null then 0 else next.hashCode()
    MurmurHash3.mix(x.hashCode(), nextHash)

def addTwoNumbersMut(l1: ListNode, l2: ListNode): ListNode =
  val dummy = ListNode()
  var cur   = dummy
  var n1    = l1
  var n2    = l2
  var carry = 0

  while n1 != null || n2 != null || carry > 0 do
    val sum  = Option(n1).fold(0)(_.x) +
      Option(n2).fold(0)(_.x) + carry
    carry    = sum / 10
    cur.next = ListNode(sum % 10)
    cur      = cur.next
    if n1 != null then n1 = n1.next
    if n2 != null then n2 = n2.next

  dummy.next

def addTwoNumbersFP(l1: ListNode, l2: ListNode): ListNode =
  @tailrec
  def loop(n1: ListNode, n2: ListNode, carry: Int, acc: ListNode): ListNode =
    (Option(n1), Option(n2)) match
      case (None, None) if carry == 0 => acc
      case _ =>
        val sum = Option(n1).fold(0)(_.x) +
          Option(n2).fold(0)(_.x) + carry
        loop(
          if n1 != null then n1.next else null,
          if n2 != null then n2.next else null,
          sum / 10,
          ListNode(sum % 10, acc)
        )

  @tailrec
  def reverse(node: ListNode, acc: ListNode): ListNode =
    Option(node) match
      case None    => acc
      case Some(n) => reverse(n.next, ListNode(n.x, acc))

  reverse(loop(l1, l2, 0, null), null)

def twoNumbersFPScala3(l1: ListNode, l2: ListNode): ListNode =
  def loop(n1: ListNode | Null, n2: ListNode | Null, carry: Int): ListNode | Null =
    (n1, n2) match
      case (null, null) if carry == 0 => null
      case _ =>
        val v1 = if n1 != null then n1.x else 0
        val v2 = if n2 != null then n2.x else 0
        val sum  = v1 + v2 + carry
        val node = ListNode(sum % 10)
        node.next = loop(
          if n1 != null then n1.next else null,
          if n2 != null then n2.next else null,
          sum / 10
        )
        node
  loop(l1, l2, 0)

@main def addTwoNumbers(): Unit =

  val ln11 = ListNode(2, ListNode(4, ListNode(3, null)))
  val ln12 = ListNode(5, ListNode(6, ListNode(4, null)))
  val out1 = ListNode(7, ListNode(0, ListNode(8, null)))

  val ln21 = ListNode(0, null)
  val ln22 = ListNode(0, null)
  val out2 = ListNode(0, null)

  val ln31 = ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, null)))))))
  val ln32 = ListNode(9, ListNode(9, ListNode(9, ListNode(9, null))))
  val out3 = ListNode(8, ListNode(9, ListNode(9, ListNode(9, ListNode(0, ListNode(0, ListNode(0, ListNode(1, null))))))))

  val data = List(
    (ln11 -> ln12) -> out1,
    (ln21 -> ln22) -> out2,
    (ln31 -> ln32) -> out3
  )

  val impl = List(addTwoNumbersMut, addTwoNumbersFP, twoNumbersFPScala3)

  for
    addTwoNumbersDef       <- impl
    ((ln1, ln2), expected) <- data
  do
    assert:
      addTwoNumbersDef(ln1, ln2) == expected
