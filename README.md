# leetcode_scala

My LeetCode solutions in Scala 3 (`3.9.0`, see [project.scala](project.scala)). Each problem is a self-contained
script that carries its own description, several solution approaches, and assertions that check them.

## Structure

```
problems/{easy,medium,hard}/   solution files, grouped by difficulty
scripts/Scaffold.scala       generator for new problem files
project.scala                  Scala CLI build config (Scala version)
```

Files are named `NNNN_PascalCaseTitle.scala`, with a zero-padded problem number, e.g. `0001_TwoSum.scala`.

## Anatomy of a solution file

- A `#!/usr/bin/env -S scala shebang` line, so the file can be executed directly
- The problem number, title, statement, examples and constraints as comments
- One function per approach, named `<problemName><Approach>` (e.g. `Loop`, `Mut`, `FP`, `Fold`)
- A `@main` with test data (`input -> expected`), the list of approaches, and a `for` comprehension that `assert`s every approach against every case

## Running a solution

Any of these runners works (Scala CLI is the reference; Scala 3.9.0 is pinned in `project.scala`):

```bash
scala-cli run problems/medium/0005_LongestPalindromicSubstring.scala   # Scala CLI
scala run problems/medium/0005_LongestPalindromicSubstring.scala       # scala runner
amm problems/medium/0005_LongestPalindromicSubstring.scala             # Ammonite
# or, thanks to the shebang:
chmod +x problems/medium/0005_LongestPalindromicSubstring.scala
./problems/medium/0005_LongestPalindromicSubstring.scala
```

A run that finishes silently means all assertions passed; a failure throws an `AssertionError`.
Ammonite uses its own bundled Scala version, which may differ from `project.scala`.

## Creating a new problem

```bash
scala-cli scripts/Scaffold.scala -- easy 1 "Two Sum"
scala-cli scripts/Scaffold.scala -- medium 2 Add Two Numbers    # quotes are optional
scala-cli scripts/Scaffold.scala -- hard 4 "Median of Two Sorted Arrays"
```

The generated file contains the header, the URL, placeholders for the statement, a
`<name>Loop` stub and the `@main` test harness. Titles starting with a digit get a `problem` prefix (`3Sum` becomes `problem3Sum`).

Options:

- `--dry-run` — print the destination path and the generated content without creating a file
- `--force` — overwrite an existing file
- `--template PATH` — use a custom template; supports `{{number}}`, `{{title}}`, `{{difficulty}}`, `{{slug}}`, `{{name}}`, `{{Name}}`
- `-h`, `--help` — show usage

Then `chmod +x` it if you want to run it via the shebang, implement the solution in the generated file and add test cases.
