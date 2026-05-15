#!/usr/bin/env amm
// Generate a new LeetCode problem file (.scala) from a built-in Scala 3 template.
// Usage: amm NewProblem.sc <difficulty> <number> <title> [--template PATH] [--force] [--dry-run]

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import $ivy.`com.lihaoyi::sourcecode:0.4.4`

val VALID_DIFFICULTIES = Set("easy", "medium", "hard")

def toPascalCase(title: String): String =
  title.strip()
    .split("[^a-zA-Z0-9]+").toSeq
    .filter(_.nonEmpty)
    .map(w => w.head.toUpper.toString + w.tail.toLowerCase)
    .mkString

def toKebabCase(title: String): String =
  title.strip().toLowerCase
    .replaceAll("[^a-z0-9]+", "-")
    .replaceAll("-+", "-")
    .stripPrefix("-")
    .stripSuffix("-")

enum Difficulty:
  case Easy, Medium, Hard

object Difficulty:
  def parse(s: String): Either[String, Difficulty] = s.toLowerCase match
    case "easy"   => Right(Difficulty.Easy)
    case "medium" => Right(Difficulty.Medium)
    case "hard"   => Right(Difficulty.Hard)
    case other    => Left(s"difficulty must be one of: ${VALID_DIFFICULTIES.mkString(", ")}, got: $other")

case class Config(
  difficulty: Option[Difficulty] = None,
  number: Int = 0,
  title: String = "",
  template: Option[Path] = None,
  force: Boolean = false,
  dryRun: Boolean = false
)

def parseArgs(args: Seq[String]): Either[String, Config] =
  def loop(remaining: Seq[String], cfg: Config): Either[String, Config] =
    remaining match
      case Nil                       => Right(cfg)
      case "--force" :: tail         => loop(tail, cfg.copy(force = true))
      case "--dry-run" :: tail       => loop(tail, cfg.copy(dryRun = true))
      case "--template" :: v :: tail => loop(tail, cfg.copy(template = Some(Paths.get(v))))
      case "--template" :: Nil       => Left("--template requires a value")
      case arg :: _ if arg.startsWith("--") => Left(s"Unknown option: $arg")
      case arg :: tail =>
        if cfg.difficulty.isEmpty then
          Difficulty.parse(arg) match
            case Left(err)  => Left(err)
            case Right(d)   => loop(tail, cfg.copy(difficulty = Some(d)))
        else if cfg.number == 0 then
          arg.toIntOption match
            case Some(n) => loop(tail, cfg.copy(number = n))
            case None    => Left(s"number must be an integer, got: $arg")
        else if cfg.title.isEmpty then
          loop(tail, cfg.copy(title = arg))
        else
          Left(s"Unexpected argument: $arg")
  loop(args, Config())

def uncapitalize(s: String): String =
  s.take(1).toLowerCase + s.drop(1)

def scalaTemplate(number: Int, title: String, difficulty: Difficulty): String =
  val objectName = toPascalCase(title)
  val urlSlug    = toKebabCase(title)
  val diff       = difficulty.toString.toLowerCase
  s"""// LeetCode $number: $title ($diff)
     |// https://leetcode.com/problems/$urlSlug/
     |
     |object $objectName:
     |
     |  // TODO: implement solution
     |  def solve(input: String): String = ???
     |
     |
     |@main def ${uncapitalize(objectName)}(): Unit =
     |  println(s"LeetCode $number: $title")
     |  val result = $objectName.solve("")
     |  println(s"Result: $$result")
     |""".stripMargin

@main def run(args: String*): Unit =
  parseArgs(args) match
    case Left(err) =>
      System.err.println(s"Error: $err")
      System.err.println(
        s"""Usage: amm NewProblem.sc <difficulty> <number> <title> [--template PATH] [--force] [--dry-run]
           |  difficulty  one of: ${VALID_DIFFICULTIES.mkString(", ")}
           |  number      LeetCode problem number, e.g. 1
           |  title       Problem title, e.g. 'Two Sum'""".stripMargin)

    case Right(cfg) =>
      (cfg.difficulty, cfg.number, cfg.title) match
        case (None, _, _) | (_, 0, _) | (_, _, "") =>
          System.err.println("Error: difficulty, number, and title are required.")
        case (Some(diff), num, title) if num <= 0 =>
          System.err.println("Error: number must be a positive integer.")
        case (Some(diff), num, title) =>
          val repoRoot   = os.Path(sourcecode.File()) / os.up / os.up
          val objectName = toPascalCase(title)
          val filename   = f"${num}%04d_$objectName.scala"
          val destDir    = Paths.get((repoRoot / "problems" / diff.toString.toLowerCase).toString)
          val destPath   = destDir.resolve(filename)

          val content: String = cfg.template match
            case Some(tplPath) =>
              if !Files.exists(tplPath) then
                System.err.println(s"Error: template not found: `$tplPath`; fallback to default template")
                scalaTemplate(num, title, diff)
              else
                Files.readString(tplPath, StandardCharsets.UTF_8)
            case None =>
              scalaTemplate(num, title, diff)

          if Files.exists(destPath) && !cfg.force then
            System.err.println(s"Error: destination already exists: $destPath\nUse --force to overwrite.")
          else if cfg.dryRun then
            println(destPath)
          else
            Files.createDirectories(destDir)
            Files.writeString(destPath, content, StandardCharsets.UTF_8)
            println(s"Created: $destPath")
