package com.melvic.lohika.core.meta

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import com.melvic.lohika.core.expression.Expression
import Expression.{Const, FunctionApp, Term, Var, given}
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.formula.Formula.PredicateApp
import Definition.{FormulaDef, TermDef}
import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.parsers.Lexemes

type Definition = TermDef | FormulaDef

object Definition:
  final case class PredId(name: String, params: List[Var])
  final case class PropId(name: String)
  final case class FuncId(name: String, params: List[Var])

  type FormulaId = PredId | PropId

  final case class TermDef(id: FuncId, term: Term)
  final case class FormulaDef(id: FormulaId, formula: Formula)

  given (using Formatter): Show[Definition] = Show.show:
    case TermDef(id, term)       => show"$id ${Lexemes.DefinedAs} $term"
    case FormulaDef(id, formula) => show"$id ${Lexemes.DefinedAs} $formula"

  given (using formatter: Formatter): Show[NonEmptyList[Definition]] = Show.show:
    _.map(_.show).toList.mkString(Lexemes.StmtDelimiter + formatter.newline)

  given (using Formatter): Show[PredId] = Show.show:
    case PredId(name, params) => PredicateApp(name, params).show

  given (using Formatter): Show[PropId] = Show.show:
    case PropId(name) => Var(name).show

  given (using Formatter): Show[FuncId] = Show.show:
    case FuncId(name, params) => FunctionApp(name, params).show

  given (using Formatter): Show[FormulaId] = Show.show:
    case predId: PredId => predId.show
    case propId: PropId => propId.show
