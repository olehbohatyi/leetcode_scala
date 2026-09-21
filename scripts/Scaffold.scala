#!/usr/bin/env -S scala shebang

// Generate a new LeetCode solution file under problems/<difficulty>/.
//
// Usage:
//   scala-cli scripts/Scaffold.scala -- <difficulty> <number> <title...> [options]
//
// Examples:
//   scala-cli scripts/Scaffold.scala -- easy 1 "Two Sum"
//   scala-cli scripts/Scaffold.scala -- medium 2 Add Two Numbers --dry-run

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.Try

// ---------------------------------------------------------------- model

enum Difficulty:
  case Easy, Medium, Hard
  def dir: String = toString.toLowerCase

object Difficulty:
  def parse(s: String): Either[String, Difficulty] =
    values.find(_.dir == s.toLowerCase).toRight(
      s"difficulty must be one of: ${values.map(_.dir).mkString(", ")}; got: $s"
    )

case class Config(
    difficulty: Difficulty,
    number: Int,
    title: String,
    template: Option[Path],
    force: Boolean,
    dryRun: Boolean
)

// ------------------------------------------------------------ naming

def words(title: String): Seq[String] =
  title.split("[^a-zA-Z0-9]+").toSeq.filter(_.nonEmpty)

def toPascalCase(title: String): String =
  words(title).map(w => w.head.toUpper.toString + w.tail).mkString

def toKebabCase(title: String): String =
  words(title).map(_.toLowerCase).mkString("-")

/** Valid Scala identifier for the `@main` entry point; "3Sum" -> "problem3Sum". */
def toFunctionName(title: String): String =
  val pascal = toPascalCase(title)
  val name   = pascal.take(1).toLowerCase + pascal.drop(1)
  if name.headOption.exists(_.isDigit) then s"problem$pascal" else name

// ---------------------------------------------------------- template

/** Placeholders: {{number}} {{title}} {{difficulty}} {{slug}} {{name}} {{Name}} */
val DefaultTemplate: String =
  """#!/usr/bin/env -S scala shebang

    |// {{number}}. {{title}}
    |// Difficulty: {{difficulty}}
    |// https://leetcode.com/problems/{{slug}}/

    |// TODO: paste the problem statement

    |// Example 1:
    |// Input:
    |// Output:
    |// Explanation:

    |// Constraints:
    |// TODO

    |// One function per approach, named {{name}}<Approach>. e.g. `Tailrec` or `Loop`.
    |def {{name}}Approach(input: Int): Int = ???

    |@main def {{name}}(): Unit =

    |  val data = List(
    |    // input -> expected
    |    0 -> 0
    |  )

    |  val impl = List(
    |    {{name}}Approach
    |  )

    |  for
    |    {{name}}Approach  <- impl
    |    (input, expected) <- data
    |  do
    |    assert:
    |      {{name}}Approach(input) == expected
    |""".stripMargin

def render(template: String, cfg: Config): String =
  Map(
    "number"     -> cfg.number.toString,
    "title"      -> cfg.title,
    "difficulty" -> cfg.difficulty.toString,
    "slug"       -> toKebabCase(cfg.title),
    "name"       -> toFunctionName(cfg.title),
    "Name"       -> toPascalCase(cfg.title)
  ).foldLeft(template):
    case (acc, (key, value)) => acc.replace(s"{{$key}}", value)

// ------------------------------------------------------------- args

val Usage: String =
  """Usage: scala-cli scripts/Scaffold.scala -- <difficulty> <number> <title...> [options]
    |
    |  difficulty      easy | medium | hard
    |  number          LeetCode problem number, e.g. 1
    |  title           problem title; quotes are optional, e.g. Two Sum
    |
    |Options:
    |  --template PATH use a custom template ({{number}} {{title}} {{difficulty}} {{slug}} {{name}} {{Name}})
    |  --force         overwrite an existing file
    |  --dry-run       print the destination path and the content, write nothing
    |  -h, --help      show this help""".stripMargin

def parseArgs(args: List[String]): Either[String, Config] =
  case class Raw(
      positional: Vector[String] = Vector.empty,
      template: Option[Path] = None,
      force: Boolean = false,
      dryRun: Boolean = false
  )

  def loop(rest: List[String], raw: Raw): Either[String, Raw] = rest match
    case Nil                       => Right(raw)
    case "--force" :: tail         => loop(tail, raw.copy(force = true))
    case "--dry-run" :: tail       => loop(tail, raw.copy(dryRun = true))
    case "--template" :: v :: tail => loop(tail, raw.copy(template = Some(Paths.get(v))))
    case "--template" :: Nil       => Left("--template requires a value")
    case opt :: _ if opt.startsWith("-") => Left(s"unknown option: $opt")
    case arg :: tail               => loop(tail, raw.copy(positional = raw.positional :+ arg))

  for
    raw <- loop(args, Raw())
    _ <- Either.cond(raw.positional.length >= 3, (), "difficulty, number and title are required")
    difficulty <- Difficulty.parse(raw.positional(0))
    number <- raw.positional(1).toIntOption.filter(_ > 0).toRight(
      s"number must be a positive integer; got: ${raw.positional(1)}"
    )
    title = raw.positional.drop(2).mkString(" ").trim
    _ <- Either.cond(words(title).nonEmpty, (), "title must contain letters or digits")
  yield Config(difficulty, number, title, raw.template, raw.force, raw.dryRun)

// -------------------------------------------------------------- main

def fail(msg: String): Nothing =
  System.err.println(s"Error: $msg")
  sys.exit(1)

@main def scaffold(args: String*): Unit =
  if args.exists(a => a == "-h" || a == "--help") then
    println(Usage)
    sys.exit(0)

  val cfg = parseArgs(args.toList).fold(err => fail(s"$err\n\n$Usage"), identity)

  val template = cfg.template match
    case Some(path) =>
      Try(Files.readString(path, StandardCharsets.UTF_8))
        .getOrElse(fail(s"cannot read template: $path"))
    case None => DefaultTemplate

  val destDir  = Paths.get("").toAbsolutePath.normalize.resolve("problems").resolve(cfg.difficulty.dir)
  val destPath = destDir.resolve(f"${cfg.number}%04d_${toPascalCase(cfg.title)}.scala")
  val content  = render(template, cfg)

  if cfg.dryRun then
    println(s"Would create: $destPath\n")
    println(content)
  else if Files.exists(destPath) && !cfg.force then
    fail(s"destination already exists: $destPath (use --force to overwrite)")
  else
    Files.createDirectories(destDir)
    Files.writeString(destPath, content, StandardCharsets.UTF_8)
    println(s"Created: $destPath")
    println(s"Run:     scala-cli run $destPath")
