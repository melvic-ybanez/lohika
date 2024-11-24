package com.melvic.lohika.parsers

import cats.data.NonEmptyList
import com.melvic.lohika.expression.Expression.{Const, FunctionApp}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.PredicateApp
import com.melvic.lohika.meta.Definition.FormulaDef
import com.melvic.lohika.meta.Entailment.{Derived, Direct}
import com.melvic.lohika.meta.{Definition, Entailment}
import fastparse.*
import fastparse.MultiLineWhitespace.*

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

  def definition[$: P]: P[Definition] = formulaDef // TODO: include term-def here

  def definitions[$: P]: P[Seq[Definition]] =
    definition.rep(min = 1, sep = Lexemes.StmtDelimiter) ~ Parser.stmtDelimiter

  def formulaDef[$: P]: P[FormulaDef] =
    P(formulaId ~ Lexemes.DefinedAs ~ Parser.formula).map(FormulaDef.apply)

  def formulaId[$: P]: P[PredicateApp] =
    Parser.predicateApp | Parser.propVar
