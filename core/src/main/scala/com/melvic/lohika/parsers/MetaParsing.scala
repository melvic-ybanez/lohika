package com.melvic.lohika.parsers

import cats.data.NonEmptyList
import com.melvic.lohika.meta.{Definition, Entailment, Identifier}
import fastparse.*
import fastparse.MultiLineWhitespace.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.meta.Entailment.{Derived, Direct}

/**
 * Parsing support for meta-logical constructs
 */
private[parsers] trait MetaParsing:
  def parseEntailment(input: String): Parsed[Entailment] =
    parse(input, entailment(using _))

  def entailment[$: P]: P[Entailment] =
    (definitions.? ~ (Parser.formula.rep(
      min = 1,
      sep = Lexemes.PremisesDelimiter
    ) ~ Lexemes.Entailment).? ~ Parser.formula ~ End).map:
      case (None, None, conclusion) => Direct(Nil, conclusion)
      case (Some(definitions), None, conclusion) =>
        Derived(NonEmptyList.fromList(definitions.toList).get, Nil, conclusion)
      case (None, Some(premises), conclusion) => Direct(premises.toList, conclusion)
      case (Some(definitions), Some(premises), conclusion) =>
        Derived(NonEmptyList.fromList(definitions.toList).get, premises.toList, conclusion)

  def definition[$: P]: P[Definition] =
    P(identifier ~ Lexemes.DefinedAs ~ Parser.expression).map(Definition.apply)

  def definitions[$: P]: P[Seq[Definition]] =
    definition.rep(min = 1, sep = Lexemes.StmtDelimiter) ~ Parser.stmtDelimiter

  def identifier[$: P]: P[Identifier] =
    Parser.predicateApp | Parser.propVar | Parser.functionApp | Parser.namedConst
