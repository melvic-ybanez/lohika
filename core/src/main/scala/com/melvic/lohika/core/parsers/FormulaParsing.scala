package com.melvic.lohika.core.parsers

import com.melvic.lohika.core.parsers.Parser.whitespace
import com.melvic.lohika.core.expression.Expression.*
import com.melvic.lohika.core.formula.Formula.*
import Parser.{alphabetic, args, firstOrderVar}
import com.melvic.lohika.core.formula.Formula
import fastparse.{P, Parsed, parse as fastParse, *}

private[parsers] trait FormulaParsing:
  def parseFormula(input: String): Parsed[Formula] =
    fastParse(input, fullFormula(using _))

  def fullFormula[$: P]: P[Formula] = formula ~ End

  def formula[$: P]: P[Formula] = P(iff)

  def quantified[$: P, Q <: Quantified](quantifier: String, make: Quantified.Make[Q])(using
      Q <:< Formula
  ): P[Formula] =
    P(quantifier ~ firstOrderVar.rep(min = 1, sep = ",") ~ highestPrecedence).map:
      case (Seq(x, xs*), matrix) => make((x, xs.toList), matrix)

  def forall[$: P]: P[Formula] = quantified(Lexemes.Forall, Forall.apply)

  def thereExists[$: P]: P[Formula] = quantified(Lexemes.ThereExists, ThereExists.apply)

  def iff[$: P]: P[Formula] =
    imply
      .rep(min = 1, sep = Lexemes.Iff)
      .map(ps => Iff.fromList(ps.toList).getOrElse(PredicateApp.False))

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

  def grouping[$: P]: P[Formula] =
    P(Lexemes.LeftParen ~ formula ~ Lexemes.RightParen) | P(
      Lexemes.LeftBracket ~ formula ~ Lexemes.RightBracket
    )

  def propVar[$: P]: P[PredicateApp] =
    P(alphabetic(_.isUpper) ~ alphabetic(_ => true).rep(min = 0).!).map: (firstChar, rest) =>
      PredicateApp.nullary(firstChar + rest)

  def predicateApp[$: P]: P[PredicateApp] = P(propVar ~ args).map:
    case (PredicateApp.Nullary(name), args) => PredicateApp(name, args)

  def highestPrecedence[$: P]: P[Formula] = P(
    forall | thereExists | grouping | not | predicateApp | propVar
  )
