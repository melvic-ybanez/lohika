package com.melvic.lohika.parsers

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.BoundVars
import fastparse.{parse as fastParse, *}
import fastparse.MultiLineWhitespace.*

object FormulaParser:
  def parse(input: String): Parsed[Formula] =
    fastParse(input, formula(using _))

  def formula[$: P]: P[Formula] = P(iff)

  def quantified[$: P, Q <: Quantified](quantifier: String, make: Quantified.Make[Q])(using
      Q <:< Formula
  ): P[Formula] =
    P(quantifier ~/ firstOrderVar.rep(min = 1, sep = ",") ~ highestPrecedence).map:
      case (Seq(x, xs*), matrix) => make((x, xs.toList), matrix)

  def forall[$: P]: P[Formula] = quantified(Lexemes.Forall, Forall.apply)

  def thereExists[$: P]: P[Formula] = quantified(Lexemes.ThereExists, ThereExists.apply)

  def iff[$: P]: P[Formula] =
    imply.rep(min = 1, sep = Lexemes.Iff).map(ps => Iff.fromList(ps.toList).getOrElse(False))

  def imply[$: P]: P[Formula] = or
    .rep(min = 1, sep = Lexemes.Imply)
    .map:
      case Seq(p) => p
      case ps     => Imply.fromList(ps.toList)

  def or[$: P]: P[Formula] = and
    .rep(min = 1, sep = Lexemes.Or)
    .map(ps => Or.fromList(ps.toList))

  def and[$: P]: P[Formula] = highestPrecedence
    .rep(min = 1, sep = Lexemes.And)
    .map(ps => And.fromList(ps.toList))

  def not[$: P]: P[Formula] = P(Lexemes.Not ~ highestPrecedence).map(Not.apply)

  def grouping[$: P]: P[Formula] = P(Lexemes.LeftParen ~ formula ~ Lexemes.RightParen)

  def propVar[$: P]: P[Var] = variable(Character.isAlphabetic)

  def firstOrderVar[$: P]: P[Var] = variable(c => Character.isAlphabetic(c) && c.isLower)

  def variable[$: P](predicate: Char => Boolean): P[Var] =
    P(CharPred(predicate).rep(min = 1).!).map(Var.apply)

  def predicate[$: P]: P[Predicate] =
    P(propVar ~ ("(" ~/ firstOrderVar.rep(min = 1, sep = ", ") ~ ")")).map:
      case (Var(name), args) => Predicate(name, args.toList)

  def highestPrecedence[$: P]: P[Formula] = P(
    forall | thereExists | grouping | not | predicate | varOrCons
  )

  def varOrCons[$: P]: P[Formula] = propVar.map:
    case Var(Lexemes.True)  => True
    case Var(Lexemes.False) => False
    case variable           => variable
