package com.melvic.lohika.expression

import cats.{Endo, Show}
import com.melvic.lohika.Formatter
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.expression.Expression.Term
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

type Expression = Formula | Term

object Expression extends ExpressionGivens with PrettyPrinting:
  type Term = Var | Const | True.type | False.type | FunctionApp

  /**
   * Represents a first-order variable. Propositional variables are considered nullary predicates,
   * represented by [[PredicateApp]] but without any arguments.
   */
  final case class Var(name: String)
  final case class Const(name: String)

  case object True
  case object False

private sealed trait ExpressionGivens:
  given showExpr[E <: Expression](using Formatter): Show[E] =
    Show.show(Expression.prettyPrint(_).formula)
