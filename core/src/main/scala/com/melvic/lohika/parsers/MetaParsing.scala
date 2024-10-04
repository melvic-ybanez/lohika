package com.melvic.lohika.parsers

import com.melvic.lohika.meta.Entailment
import fastparse.*
import fastparse.MultiLineWhitespace.*

/**
 * Parser for meta-logical constructs
 */
private[parsers] trait MetaParsing:
  def parseEntailment(input: String): Parsed[Entailment] =
    parse(input, entailment(using _))

  def entailment[$: P]: P[Entailment] =
    val entailment =
      ((Parser.formula.rep(min = 1, sep = ",") ~ "|=").? ~ Parser.formula).map:
        case (None, conclusion)           => Entailment(Nil, conclusion)
        case (Some(premises), conclusion) => Entailment(premises.toList, conclusion)
    entailment ~ End
