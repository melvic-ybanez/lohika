package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import com.melvic.lohika.Formula.{Or, Var}

object Parser:
  def variable[$: P]: P[Var] = P(CharPred(Character.isAlphabetic).rep(min = 1).!.map(Var.apply))

  def parens[$: P]: P[Formula] = P("(" ~/ (or | variable) ~ ")")

  def or[$: P]: P[Formula] = P(variable | parens)
    .rep(min = 1, sep = "|")
    .map:
      case Seq(variable) => variable
      case or            => Or.fromSeq(or)

  def formula[$: P]: P[Formula] = P(or)

  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))
