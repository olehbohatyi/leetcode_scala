# leetcode_scala

Scala 3 project for solving LeetCode problems.

## Structure

- `problems/easy`, `problems/medium`, `problems/hard` — solution files grouped by difficulty
- `src/leetcode/templates` — reusable Scala 3 starter template for new problems
- `tests` — unit tests (e.g. with MUnit or ScalaTest)
- `scripts` — utility scripts

## File naming

Files follow the Scala naming convention — `PascalCase.scala` with a zero-padded number prefix for ordering:

- `problems/easy/0001_TwoSum.scala`
- `problems/medium/0002_AddTwoNumbers.scala`

## Quick start

1. Create a new problem file in the appropriate difficulty folder using the generator (see below).
2. Open the generated `.scala` file and implement your solution inside `object SolutionName`.
3. Run the file with Scala CLI: `scala-cli run problems/easy/0001_TwoSum.scala`
4. Optionally add tests in `tests/`.

## Generated file structure

Each generated file is a valid, runnable Scala 3 file:

```scala
// LeetCode 1: Two Sum (easy)
// https://leetcode.com/problems/two-sum/

object TwoSum:

  // TODO: implement solution
  def solve(input: String): String = ???


@main def twoSum(): Unit =
  println(s"LeetCode 1: Two Sum")
  val result = TwoSum.solve("")
  println(s"Result: $result")
```

## Problem generator CLI

Use the Ammonite script to create new problem files automatically:

```bash
amm scripts/NewProblem.sc easy 1 "Two Sum"
amm scripts/NewProblem.sc medium 2 "Add Two Numbers"
```

Useful flags:

- `--dry-run` — prints the destination path without creating a file
- `--force` — overwrites an existing file
- `--template PATH` — use a custom `.scala` template file instead of the built-in one

## Requirements

- [Scala CLI](https://scala-cli.virtuslab.org/) — to run `.scala` files
- [Ammonite](https://ammonite.io/) — to run the generator script (`amm`)
