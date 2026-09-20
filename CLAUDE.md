# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

LeetCode solutions in Scala 3 (version pinned in `project.scala`), built with [Scala CLI](https://scala-cli.virtuslab.org/). There is no sbt build, test framework, or linter. Each problem file is a standalone script.

## Commands

```bash
# Run a solution (silent exit = all assertions passed; failure throws AssertionError)
scala-cli run problems/medium/0005_LongestPalindromicSubstring.scala
# Equivalent runners: `scala run <file>` and `amm <file>` (Ammonite, uses its own bundled Scala version)
# Files have a shebang, so after chmod +x they can also be run directly
./problems/medium/0005_LongestPalindromicSubstring.scala

# Create a new problem file (options: --dry-run, --force, --template PATH, --help)
scala-cli scripts/Scaffold.scala -- medium 2 Add Two Numbers
```

"Running a single test" means running that one file; there is no aggregate test command.

## Structure and conventions

- `problems/{easy,medium,hard}/NNNN_PascalCaseTitle.scala` — zero-padded problem number, grouped by difficulty.
- `scripts/Scaffold.scala` generates new files from a built-in template (`DefaultTemplate`, with `{{placeholders}}` filled by `render`); prefer it over hand-creating files so the layout stays consistent. Change the layout there when the convention evolves.
- Each solution file follows the same layout:
  1. `#!/usr/bin/env -S scala shebang` line
  2. Problem statement, examples and constraints as `//` comments
  3. Several top-level functions, one per approach, named `<problemName><Approach>` (e.g. `Loop`, `Mut`, `FP`, `Indexes`)
  4. A `@main` function holding test data (`List(input -> expectedSet)`) and a `for` comprehension that `assert`s every approach against every case. Expected values are `Set`s when multiple answers are valid.
- Code style: Scala 3 indentation syntax (`then`/`do`, `:` for blocks, no braces), `@tailrec` for the functional variants. Adding a new approach means adding a function and appending it to the `impl` list in `@main`.
