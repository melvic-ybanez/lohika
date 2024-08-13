package com.melvic.lohika

import fastparse.*
import MultiLineWhitespace.*
import com.melvic.lohika.Formula.{Or, Var}

object Parser:
  def variable[$: P]: P[Var] = P(CharPred(Character.isAlphabetic).!.map(Var.apply))

  def or[$: P]: P[Formula] = variable.rep(min = 1, sep = "|").map:
    case Seq(variable) => variable
    case or => Or.fromSeq(or)

  def formula[$: P]: P[Formula] = P(or | ("(" ~ or ~ ")"))
