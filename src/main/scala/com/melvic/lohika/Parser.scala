package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import com.melvic.lohika.Formula.{And, Or, Var}

object Parser:
  def variable[$: P]: P[Var] = P(CharPred(Character.isAlphabetic).rep(min = 1).!.map(Var.apply))

  def parens[$: P]: P[Formula] = P("(" ~ (or | variable) ~ ")")

  def or[$: P]: P[Formula] = P(variable | parens)
    .rep(min = 1, sep = "|")
    .map:
      case Seq(formula) => formula
      case or           => Or.fromSeq(or)

  def and[$: P]: P[Formula] = P(or | ("(" ~ and ~ ")"))
    .rep(min = 1, sep = "&")
    .map:
      case Seq(formula) => formula
      case and          => And.fromSeq(and)

  def formula[$: P]: P[Formula] = P(and)

  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))
