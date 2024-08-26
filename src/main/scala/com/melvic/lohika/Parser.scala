package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import com.melvic.lohika.Formula.*

object Parser:
  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))

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

  def and[$: P]: P[Formula] = P(variable | P("(" ~ (formula | variable) ~ ")"))
    .rep(min = 1, sep = "&")
    .map(ps => And.fromList(ps.toList))

  def variable[$: P]: P[Formula] = P(CharPred(Character.isAlphabetic).rep(min = 1).!).map:
    case "T"  => True
    case "F"  => False
    case name => Var(name)
