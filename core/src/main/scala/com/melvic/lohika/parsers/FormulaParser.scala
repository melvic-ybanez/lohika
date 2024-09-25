package com.melvic.lohika.parsers

import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantification.QVars
import fastparse.{parse as fastParse, *}
import fastparse.MultiLineWhitespace.*

object FormulaParser:
  def parse(input: String): Parsed[Formula] =
    fastParse(input, formula(using _))

  def formula[$: P]: P[Formula] = P(iff)

  def quantification[$: P, Q <: Quantification](quantifier: String, make: Quantification.Make[Q])(
      using Q <:< Formula
  ): P[Formula] =
    P(quantifier ~/ variable.rep(min = 1, sep = ",") ~ inParens).map:
      case (Seq(x, xs*), matrix) => make((x, xs.toList), matrix)

  def forall[$: P]: P[Formula] = quantification(Lexemes.Forall, Forall.apply)

  def exists[$: P]: P[Formula] = quantification(Lexemes.Exists, Exists.apply)

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

  def inParens[$: P]: P[Formula] = P(Lexemes.LeftParen ~ formula ~ Lexemes.RightParen)

  def variable[$: P]: P[Var] = P(CharPred(Character.isAlphabetic).rep(min = 1).!).map(Var.apply)

  def highestPrecedence[$: P]: P[Formula] = P(forall | exists | inParens | not | varOrCons)

  def varOrCons[$: P]: P[Formula] = variable.map:
    case Var(Lexemes.True)  => True
    case Var(Lexemes.False) => False
    case variable           => variable
