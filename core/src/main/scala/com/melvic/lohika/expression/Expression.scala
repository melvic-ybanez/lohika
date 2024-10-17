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

  object Term:
    /**
     * Unifies the two terms.
     *
     * Note: This isn't a fail-fast implementation
     */
    def unify: Endo[(Term, Term)] =
      case (_: Var, term) => (term, term)
      case (term, _: Var) => (term, term)
      case (FunctionApp(f, fArgs), FunctionApp(g, gArgs)) =>
        val (newFArgs, newGArgs) = unifyApp((f, fArgs), (g, gArgs))
        (FunctionApp(f, newFArgs), FunctionApp(g, gArgs))
      case terms => terms

  def unifyApp: ((String, List[Term]), (String, List[Term])) => (List[Term], List[Term]) =
    case ((f, fArgs), (g, gArgs)) if f == g && fArgs.length == gArgs.length =>
      fArgs.zip(gArgs).map(Term.unify).unzip
    case ((_, fArgs), (_, gArgs)) => (fArgs, gArgs)

private sealed trait ExpressionGivens:
  given showExpr[E <: Expression](using Formatter): Show[E] =
    Show.show(Expression.prettyPrint(_).formula)
