package com.melvic.lohika.expression

import cats.*
import cats.implicits.*
import com.melvic.lohika.Formatter
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.expression.Expression.{Term, Var}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

import scala.collection.immutable.Set

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

  def freeVarNames(using quantifiedNames: Set[String]): Expression => Set[String] =
    collect:
      case Var(x) if !quantifiedNames.contains(x) => Set(x)
      case PredicateApp(_, args)                  => args.map(freeVarNames).combineAll
      case Quantified(_, (Var(x), xs), matrix) =>
        freeVarNames(using (x :: xs.map(_.name)).toSet ++ quantifiedNames)(matrix)

  def functionNames: Expression => Set[String] =
    collect:
      case FunctionApp(name, _) => Set(name)

  def collect[F[_], A](f: PartialFunction[Expression, F[A]])(using
      monoid: Monoid[F[A]]
  ): Expression => F[A] =
    case fm if f.isDefinedAt(fm) => f(fm)
    case FList(p, q, rs) => collect(f)(p) |+| collect(f)(q) |+| rs.map(collect(f)).combineAll
    case Imply(p, q)     => collect(f)(p) |+| collect(f)(q)
    case Not(p)          => collect(f)(p)
    case fm              => Monoid[F[A]].empty

private sealed trait ExpressionGivens:
  given showExpr[E <: Expression](using Formatter): Show[E] =
    Show.show(Expression.prettyPrint(_).formula)
