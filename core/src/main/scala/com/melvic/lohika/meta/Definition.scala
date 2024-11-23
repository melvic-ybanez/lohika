package com.melvic.lohika.meta

import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter
import com.melvic.lohika.expression.Expression
import com.melvic.lohika.expression.Expression.{Const, given}
import com.melvic.lohika.formula.Formula.{FunctionApp, PredicateApp}
import com.melvic.lohika.parsers.Lexemes

final case class Definition(identifier: Identifier, expression: Expression)

object Definition:
  given (using Formatter): Show[Definition] = Show.show:
    case Definition(id, expr) => show"$id ${Lexemes.DefinedAs} $expr"

/**
 * Abstractions over expressions.
 *
 * Note: [[Const]] could also just be replaced by nullary functions, and therefore removed here,
 * like how propositional vars are nullary predicates, but let's just keep things as they are for
 * now.
 */
type Identifier = PredicateApp | FunctionApp | Const

object Identifier:
  given (using Formatter): Show[Identifier] = Show.show:
    case pred: PredicateApp => pred.show
    case func: FunctionApp => func.show
    case const: Const => const.show
