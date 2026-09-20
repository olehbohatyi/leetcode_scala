# leetcode_scala

My LeetCode solutions in Scala 3 (`3.9.0`, see [project.scala](project.scala)). Each problem is a self-contained
script that carries its own description, several solution approaches, and assertions that check them.

## Structure

```
problems/{easy,medium,hard}/   solution files, grouped by difficulty
scripts/NewProblem.scala       generator for new problem files
project.scala                  Scala CLI build config (Scala version)
```

Files are named `NNNN_PascalCaseTitle.scala`, with a zero-padded problem number, e.g. `0001_TwoSum.scala`.

## Anatomy of a solution file

- A `#!/usr/bin/env -S scala shebang` line, so the file can be executed directly
- The problem statement, examples and constraints as comments
- One function per approach (e.g. loop, mutable, functional, index-based)
- Test data plus `assert`s that run every approach against every case

## Running a solution

Requires [Scala CLI](https://scala-cli.virtuslab.org/).

```bash
scala-cli run problems/medium/0005_LongestPalindromicSubstring.scala
# or, thanks to the shebang:
chmod +x problems/medium/0005_LongestPalindromicSubstring.scala
./problems/medium/0005_LongestPalindromicSubstring.scala
```

A run that finishes silently means all assertions passed; a failure throws an `AssertionError`.

## Creating a new problem

```bash
scala-cli scripts/NewProblem.scala -- easy 1 "Two Sum"
scala-cli scripts/NewProblem.scala -- medium 2 "Add Two Numbers"
```

Options:

- `--dry-run` — print the destination path without creating a file
- `--force` — overwrite an existing file
- `--template PATH` — use a custom `.scala` template instead of the built-in one

Then implement the solution in the generated file and add test cases.
