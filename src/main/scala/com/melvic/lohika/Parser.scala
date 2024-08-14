package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import com.melvic.lohika.Formula.{And, Iff, Imply, Or, Var}

object Parser:
  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))

  def formula[$: P]: P[Formula] = P(iff)

  def iff[$: P]: P[Formula] = P(P(imply ~ "<=>" ~ imply).map(Iff.apply) | imply)

  def imply[$: P]: P[Formula] =
    P(or ~ (P("=>") ~ or).rep(min = 0)).map:
      case (or, Seq()) => or
      case (p, qs) => Imply.fromSeq(p +: qs)

  def or[$: P]: P[Formula] = P(and | ("(" ~ or ~ ")"))
    .rep(min = 1, sep = "|")
    .map:
      case Seq(formula) => formula
      case or => Or.fromSeq(or)

  def and[$: P]: P[Formula] = P(variable | parens)
    .rep(min = 1, sep = "&")
    .map:
      case Seq(formula) => formula
      case and => And.fromSeq(and)

  def variable[$: P]: P[Var] = P(CharPred(Character.isAlphabetic).rep(min = 1).!.map(Var.apply))

  def parens[$: P]: P[Formula] = P("(" ~ (and | variable) ~ ")")
