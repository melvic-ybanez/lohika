package com.melvic.lohika.meta

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import com.melvic.lohika.Formatter
import com.melvic.lohika.expression.Expression
import com.melvic.lohika.expression.Expression.{Const, FunctionApp, Term, given}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.PredicateApp
import com.melvic.lohika.meta.Definition.{FormulaDef, TermDef}
import com.melvic.lohika.parsers.Lexemes

type Definition = TermDef | FormulaDef

object Definition:
  final case class TermDef(id: Const | FunctionApp, term: Term)
  final case class FormulaDef(id: PredicateApp, formula: Formula)

  given (using Formatter): Show[Definition] = Show.show:
    case TermDef(id, term)       => show"$id ${Lexemes.DefinedAs} $term"
    case FormulaDef(id, formula) => show"$id ${Lexemes.DefinedAs} $formula"

  given (using formatter: Formatter): Show[NonEmptyList[Definition]] = Show.show:
    _.map(_.show).toList.mkString(Lexemes.StmtDelimiter + formatter.newline)
