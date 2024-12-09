package com.melvic.lohika.parsers

import cats.data.NonEmptyList
import com.melvic.lohika.expression.Expression.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.PredicateApp
import com.melvic.lohika.meta.Definition.*
import com.melvic.lohika.meta.Entailment.{Derived, Direct}
import com.melvic.lohika.meta.{Definition, Entailment}
import fastparse.*
import fastparse.ScriptWhitespace.*

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

  def definition[$: P]: P[Definition] = formulaDef | termDef

  def definitions[$: P]: P[Seq[Definition]] =
    definition.rep(min = 1, sep = Lexemes.StmtDelimiter) ~ Parser.stmtDelimiter

  def formulaDef[$: P]: P[FormulaDef] =
    P(formulaId ~ Lexemes.DefinedAs ~ Parser.formula).map(FormulaDef.apply)

  def termDef[$: P]: P[TermDef] =
    P(funcId ~ Lexemes.DefinedAs ~ Parser.term).map(TermDef.apply)

  def propId[$: P]: P[PropId] =
    Parser.propVar.map(app => PropId(app.name))

  def predId[$: P]: P[PredId] = P(Parser.propVar ~ Parser.params).map:
    case (PredicateApp.Nullary(name), params) => PredId(name, params)

  def formulaId[$: P]: P[FormulaId] = predId | propId

  def funcId[$: P]: P[FuncId] = P(Parser.firstOrderVar ~ Parser.params).map:
    case (Var(name), params) => FuncId(name, params)
