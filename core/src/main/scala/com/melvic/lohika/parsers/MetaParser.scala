package com.melvic.lohika.parsers

import com.melvic.lohika.Entailment
import fastparse.*
import fastparse.MultiLineWhitespace.*

/**
 * Parser for meta-logical constructs
 */
object MetaParser:
  def parseEntailment(input: String): Parsed[Entailment] =
    parse(input, entailment(using _))

  def entailment[$: P]: P[Entailment] =
    val entailment =
      ((FormulaParser.formula.rep(min = 1, sep = ",") ~ "|=").? ~ FormulaParser.formula).map:
        case (None, conclusion)           => Entailment(Nil, conclusion)
        case (Some(premises), conclusion) => Entailment(premises.toList, conclusion)
    entailment ~ End
