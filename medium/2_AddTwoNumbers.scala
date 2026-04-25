import scala.annotation.tailrec
//  2. Add Two Numbers
//
//  You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order, and each of their nodes contains a single digit. Add the two numbers and return the sum as a linked list.
//
//  You may assume the two numbers do not contain any leading zero, except the number 0 itself.
//
//  Example 1:
//
//  Input: l1 = [2,4,3], l2 = [5,6,4]
//  Output: [7,0,8]
//  Explanation: 342 + 465 = 807.
//
//  Example 2:
//  Input: l1 = [0], l2 = [0]
//  Output: [0]
//
//  Example 3:
//
//  Input: l1 = [9,9,9,9,9,9,9], l2 = [9,9,9,9]
//  Output: [8,9,9,9,0,0,0,1]
//
//  Constraints:
//
//  The number of nodes in each linked list is in the range [1, 100].
//  0 <= Node.val <= 9
//  It is guaranteed that the list represents a number that does not have leading zeros.

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

def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
  @tailrec
  def loop(reminder: Int, ln1: ListNode, ln2: ListNode, acc: ListNode): ListNode =
    (Option(ln1), Option(ln2)) match {
      case (None, None)           =>
        if (reminder == 0) acc
        else ListNode(reminder, acc)
      case (Some(ln1), None)      =>
        val (res, add) = ln1.x + reminder match {
          case x if x >= 10 => (x % 10, 1)
          case x => (x, 0)
        }
        loop(add, ln1.next, null, ListNode(res, acc))
      case (None, Some(ln2))      =>
        val (res, add) = ln2.x + reminder match {
          case x if x >= 10 => (x % 10, 1)
          case x => (x, 0)
        }
        loop(add, null, ln2.next, ListNode(res, acc))
      case (Some(ln1), Some(ln2)) =>
        val (res, add) = ln1.x + ln2.x + reminder match {
          case x if x >= 10 => (x % 10, 1)
          case x => (x, 0)
        }
        loop(add, ln1.next, ln2.next, ListNode(res, acc))
    }
  val res = loop(0, l1, l2, null)
  @tailrec
  def reverse(listNode: ListNode, acc: ListNode): ListNode =
    Option(listNode) match {
      case None => acc
      case Some(s) => reverse(s.next, ListNode(s.x, acc))
    }

  reverse(res.next, ListNode(res.x, null))
}

@main def atn(): Unit =
  val node1 = ListNode(2, ListNode(4, ListNode(3, null)))
  val node2 = ListNode(5, ListNode(6, ListNode(4, null)))
  println(addTwoNumbers(new ListNode(0, null), new ListNode(0, null)))
  println(addTwoNumbers(node1, node2))