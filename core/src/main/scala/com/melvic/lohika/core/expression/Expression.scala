package com.melvic.lohika.core.expression

import cats.*
import cats.implicits.*
import com.melvic.lohika.core.Formatter.*
import com.melvic.lohika.core.expression.Expression.Term
import com.melvic.lohika.core.formula.Cnf.CLiteral
import com.melvic.lohika.core.formula.Formula
import com.melvic.lohika.core.formula.Formula.*
import com.melvic.lohika.core.meta.Definition
import com.melvic.lohika.core.meta.Definition.*
import com.melvic.lohika.core.{Formatter, rewriteOrId}

import scala.annotation.tailrec
import scala.collection.immutable.Set

type Expression = Formula | Term

object Expression extends ExpressionGivens with PrettyPrinting:
  type Term = Var | Const | True.type | False.type | FunctionApp

  type Substitute[A] = A => (Var, Term) => A

  final case class FunctionApp(name: String, args: List[Term])

  /**
   * Represents a first-order variable. Propositional variables are considered nullary predicates,
   * represented by [[PredicateApp]] but without any arguments.
   */
  final case class Var(name: String)
  final case class Const(name: String) // maybe this could also just be a nullary function?

  case object True
  case object False

  def substitute[E <: Expression]: Substitute[Expression] =
    case expr: Term  => Term.substitute(expr)
    case fm: Formula => Formula.substitute(fm)

  def unifyApp: ((String, List[Term]), (String, List[Term])) => Mgu =
    case ((f, fArgs), (g, gArgs)) if f == g && fArgs.length == gArgs.length =>
      @tailrec
      def unify(argPairs: List[(Term, Term)], mgu: Mgu): Mgu =
        argPairs match
          case Nil => mgu
          case (fArg, gArg) :: rest =>
            val newMgu = Term.unify(fArg, gArg) ++ mgu
            if newMgu.isEmpty then Map.empty
            else
              val newRest = rest.map: (fArg, gArg) =>
                val applyMgu = Mgu.applyToTerm(newMgu)
                (applyMgu(fArg), applyMgu(gArg))
              unify(newRest, newMgu)
      unify(fArgs.zip(gArgs), Map.empty)
    case _ => Map.empty

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

  object Term:
    /**
     * Unifies the two terms.
     */
    def unify: (Term, Term) => Mgu =
      case (v: Var, term) => Map(v -> term)
      case (term, v: Var) => Map(v -> term)
      case (FunctionApp(f, fArgs), FunctionApp(g, gArgs)) =>
        unifyApp((f, fArgs), (g, gArgs))
      case _ => Map.empty

    def substitute: Substitute[Term] =
      case v @ Var(name) => (variable, term) => if name == variable.name then term else v
      case FunctionApp(name, args) =>
        (variable, term) => FunctionApp(name, args.map(substitute(_)(variable, term)))
      case term: Term => (_, _) => term

    def unfold(using definitions: List[Definition]): Endo[Term] = rewriteOrId:
      case function @ FunctionApp(name, args) =>
        val unfoldedArgs = args.map(unfold)
        definitions
          .collectFirst { case TermDef(FuncId(`name`, params), term) =>
            unfold(
              params
                .zip(unfoldedArgs)
                .foldLeft(term):
                  case (term, (param, arg)) => Term.substitute(term)(param, arg)
            )
          }
          .getOrElse(FunctionApp(name, unfoldedArgs))

  object FunctionApp:
    def unary(name: String, arg: Term): FunctionApp =
      FunctionApp(name, arg :: Nil)

  /**
   * Most General Unifier
   */
  opaque type Mgu = Map[Var, Term]

  object Mgu:
    type ApplyAll[A] = Mgu => Endo[A]

    def isEmpty: Mgu => Boolean =
      _.isEmpty

    def applyToTerm: ApplyAll[Term] =
      applyAll(Term.substitute)

    def applyToLiteral: ApplyAll[CLiteral] =
      Mgu.applyAll(CLiteral.substitute)

    def applyToPredicate: ApplyAll[PredicateApp] =
      Mgu.applyAll(PredicateApp.substitute)

    def applyAll[A](substitute: Substitute[A]): ApplyAll[A] =
      mgu =>
        mgu.foldLeft(_):
          case (acc, (variable, term)) =>
            substitute(acc)(variable, term)

private trait ExpressionGivens:
  given showExpr[E <: Expression](using Formatter): Show[E] =
    Show.show(Expression.prettyPrint(_).formula)
