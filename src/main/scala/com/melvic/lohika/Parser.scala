package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import cats.Applicative
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import cats.implicits.*

object Parser:
  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))

  def parseFormulae(input: String): Parsed[List[Formula]] =
    input.split(",").toList.traverse(fm => parseFormula(fm.trim))

  def formula[$: P]: P[Formula] = P(iff)

  def iff[$: P]: P[Formula] =
    imply.rep(min = 1, sep = "<=>").map(ps => Iff.fromList(ps.toList).getOrElse(False))

  def imply[$: P]: P[Formula] = or
    .rep(min = 1, sep = "=>")
    .map:
      case Seq(p) => p
      case ps     => Imply.fromList(ps.toList)

  def or[$: P]: P[Formula] = and
    .rep(min = 1, sep = "|")
    .map(ps => Or.fromList(ps.toList))

  def and[$: P]: P[Formula] = P(inParens | not | variable)
    .rep(min = 1, sep = "&")
    .map(ps => And.fromList(ps.toList))

  def not[$: P]: P[Formula] = P("!" ~ (variable | not | inParens)).map(Not.apply)

  def inParens[$: P]: P[Formula] = P("(" ~ formula ~ ")")

  def variable[$: P]: P[Formula] = P(CharPred(Character.isAlphabetic).rep(min = 1).!).map:
    case "T"  => True
    case "F"  => False
    case name => Var(name)

  /**
   * WARNING: Applicative Laws were not verified
   */
  given parserApp: Applicative[Parsed] with
    override def pure[A](x: A): Parsed[A] =
      Parsed.Success(x, 0)

    override def ap[A, B](ff: Parsed[A => B])(fa: Parsed[A]): Parsed[B] =
      (fa, ff) match
        case (Parsed.Success(a, index), Parsed.Success(f, _)) => Parsed.Success(f(a), index)
        case (failure: Parsed.Failure, _)                     => failure
        case (_, failure: Parsed.Failure)                     => failure
