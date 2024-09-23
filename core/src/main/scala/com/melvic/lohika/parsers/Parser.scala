package com.melvic.lohika.parsers

import com.melvic.lohika.Entailment
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import fastparse.*
import fastparse.MultiLineWhitespace.*

object Parser:
  def parseEntailment(input: String): Parsed[Entailment] =
    parse(input, entailment(using _))

  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))

  def entailment[$: P]: P[Entailment] =
    val entailment = ((formula.rep(min = 1, sep = ",") ~ "|=").? ~ formula).map:
      case (None, conclusion)           => Entailment(Nil, conclusion)
      case (Some(premises), conclusion) => Entailment(premises.toList, conclusion)
    entailment ~ End

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
