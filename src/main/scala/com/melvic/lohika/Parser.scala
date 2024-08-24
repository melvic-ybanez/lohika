package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import com.melvic.lohika.Formula.*

object Parser:
  def parseFormula(input: String): Parsed[Formula] =
    parse(input, formula(using _))

  def formula[$: P]: P[Formula] = P(iff)

  def iff[$: P]: P[Formula] = P(P(imply ~ "<=>" ~ imply).map(Iff.apply) | imply)

  def imply[$: P]: P[Formula] =
    P(or ~ (P("=>") ~ or).rep(min = 0)).map:
      case (or, Seq()) => or
      case (p, qs)     => Imply.fromSeq(p +: qs)

  def or[$: P]: P[Formula] = P(and | ("(" ~ or ~ ")"))
    .rep(min = 1, sep = "|")
    .map:
      case Seq(p)         => p
      case Seq(p, q, rs*) => Or.of(p, q, rs*)

  def and[$: P]: P[Formula] = P(variable | parens)
    .rep(min = 1, sep = "&")
    .map:
      case Seq(p)         => p
      case Seq(p, q, rs*) => And.of(p, q, rs*)

  def variable[$: P]: P[Formula] = P(CharPred(Character.isAlphabetic).rep(min = 1).!).map:
    case "T"  => True
    case "F"  => False
    case name => Var(name)

  def parens[$: P]: P[Formula] = P("(" ~ (and | variable) ~ ")")
