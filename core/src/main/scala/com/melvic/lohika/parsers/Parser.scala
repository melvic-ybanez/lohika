package com.melvic.lohika.parsers

import com.melvic.lohika.expression.Expression.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*
import com.melvic.lohika.formula.Formula.Quantified.BoundVars
import fastparse.{parse as fastParse, *}
import fastparse.MultiLineWhitespace.*

object Parser extends MetaParsing with ExprParsing with FormulaParsing:
  def alphabetic[$: P](filter: Char => Boolean): P[String] =
    CharPred(c => Character.isAlphabetic(c) && filter(c)).!
