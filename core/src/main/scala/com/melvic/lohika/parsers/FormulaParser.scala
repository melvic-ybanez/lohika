package com.melvic.lohika.parsers

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.meta.Entailment
import fastparse.{parse => fastParse, *}
import fastparse.MultiLineWhitespace.*

object FormulaParser:
  def parse(input: String): Parsed[Formula] =
    fastParse(input, formula(using _))

  def formula[$: P]: P[Formula] = P(iff)

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

  def and[$: P]: P[Formula] = P(inParens | not | variable)
    .rep(min = 1, sep = Lexemes.And)
    .map(ps => And.fromList(ps.toList))

  def not[$: P]: P[Formula] = P(Lexemes.Not ~ (variable | not | inParens)).map(Not.apply)

  def inParens[$: P]: P[Formula] = P(Lexemes.LeftParen ~ formula ~ Lexemes.RightParen)

  def variable[$: P]: P[Formula] = P(CharPred(Character.isAlphabetic).rep(min = 1).!).map:
    case Lexemes.True  => True
    case Lexemes.False => False
    case name          => Var(name)
