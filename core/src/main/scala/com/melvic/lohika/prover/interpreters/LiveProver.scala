package com.melvic.lohika.prover.interpreters

import cats.Functor
import cats.data.WriterT
import cats.implicits.*
import com.melvic.lohika.Formatter
import com.melvic.lohika.Formatter.*
import com.melvic.lohika.expression.Expression
import com.melvic.lohika.formula.Cnf.*
import com.melvic.lohika.formula.Formula.{ConstSuffix, PredicateApp}
import com.melvic.lohika.formula.{Clauses, Cnf, Formula}
import com.melvic.lohika.meta.{Entailment, Equivalence}
import com.melvic.lohika.parsers.Parser
import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.prover.algebras.Prover.{
  Contradiction,
  Derived,
  Exhaustion,
  ResolutionResult
}
import fastparse.Parsed

import scala.annotation.tailrec

object LiveProver:
  import Clauses.given
  import com.melvic.lohika.Givens.given

  type Steps[X] = WriterT[[R] =>> Either[String, R], List[String], X]

  given liveProver: Prover[Steps] with
    override def splitAllIntoClauses(cnfs: List[Cnf]): Steps[Clauses] =
      if cnfs.isEmpty then WriterT((Nil, Clauses.empty).asRight)
      else step(show"${itemNumber}Extract all the clauses from $cnfs", Clauses.fromCnfs(cnfs))

    override def convertAllToCnfs(formulae: List[Formula])(using
        constSuffix: ConstSuffix
    ): Steps[List[Cnf]] =
      formulae match
        case Nil => subStep("No formulae to convert".emphasize, Nil)
        case _ =>
          val cnfsString = if formulae.size > 1 then "their CNFs" else "its CNF"
          val cnfs = formulae.zipWithIndex.foldLeft(
            List(show"$indent* Convert $formulae into $cnfsString:"),
            List.empty[Cnf]
          ):
            case (step, (formula, i)) =>
              given ConstSuffix = ConstSuffix(i + constSuffix.raw)
              step.flatMap: cnfs =>
                val cnf = Formula.toCnf(formula)
                (s"$indent$indent* " + Equivalence(formula, cnf).show :: Nil, cnf :: cnfs)

          WriterT(cnfs.asRight)

    override def updateClauseSet(clauseSet: Clauses, newClauses: Clauses): Steps[Clauses] =
      val newClauseSet = clauseSet ++ newClauses
      if newClauseSet.isEmpty then subStep("Empty clause set".emphasize, newClauseSet)
      else
        write(show"Add $newClauses to the clause set. Here's the new clause set:")
          .flatMap(_ => subStep(newClauseSet.show, newClauseSet))

    override def transform(lhs: Formula, rhs: Formula): Steps[Formula] =
      subStep(show"$lhs becomes $rhs", rhs)

    override def resolve(clauseSet: Clauses): Steps[ResolutionResult] =
      @tailrec
      def outerLoop(clauses: Clauses, result: ResolutionResult): ResolutionResult =
        clauses.toList match
          case Nil => result
          case clause :: rest =>
            def innerLoop(
                pairs: List[(Clause, Clause)],
                result: ResolutionResult
            ): ResolutionResult =
              pairs match
                case Nil => result
                case (left, right) :: rest =>
                  resolvePair(left, right).fold(innerLoop(rest, result)):
                    case contradiction: Contradiction => contradiction
                    case Derived(_, _, clause) if clauseSet.contains(clause) =>
                      innerLoop(rest, result)
                    case derived => innerLoop(rest, derived)

            innerLoop(rest.map(clause -> _), result) match
              case contradiction: Contradiction => contradiction
              case altResult                    => outerLoop(Clauses(rest*), altResult)
      step(outerLoop(clauseSet, Exhaustion))

    override def write(description: String): Steps[Unit] =
      step(s"${itemNumber}$description", ())

    override def parseEntailment(rawEntailment: String): Steps[Entailment] =
      Parser.parseEntailment(rawEntailment) match
        case Parsed.Success(entailment, _)   => step(entailment)
        case Parsed.Failure(label, _, extra) => WriterT(s"Unable to parse '$rawEntailment'.".asLeft)

  def step[A](description: String, value: A): Steps[A] =
    WriterT((description :: Nil, value).asRight)

  def subStep[A](description: String, value: A): Steps[A] =
    step(s"$indent* $description", value)

  def step[A](value: A): Steps[A] = WriterT((Nil, value).asRight)

  def resolvePair: (Clause, Clause) => Option[Derived | Contradiction] =
    case (lit1: CLiteral, lit2: CLiteral) => complementary(lit1, lit2).map(Contradiction(_, _))
    case (lit: CLiteral, or: COr)         => resolvePair(COr(lit :: Nil), or)
    case (or: COr, lit: CLiteral)         => resolvePair(or, COr(lit :: Nil))
    case (cor1 @ COr(literals1), cor2 @ COr(literals2)) =>
      literals1
        .collectFirstSome(lit1 => literals2.collectFirstSome(complementary(lit1, _)))
        .map: (lit1, lit2) =>
          val newLiterals1 = literals1.filterNot(_ == lit1)
          val newLiterals2 = literals2.filterNot(_ == lit2)
          val cOrLiterals = newLiterals1 ++ newLiterals2

          if cOrLiterals.isEmpty then Contradiction(lit1, lit2)
          else Derived(cor1, cor2, COr(cOrLiterals))

  def complementary: (CLiteral, CLiteral) => Option[(CLiteral, CLiteral)] =
    case (p1: PredicateApp, c2 @ CNot(p2: PredicateApp)) if unifyCompare(p1, p2) => Some(p1, c2)
    case (c1 @ CNot(p1: PredicateApp), p2: PredicateApp) if unifyCompare(p1, p2) => Some(c1, p2)
    case _                                                                       => None

  def unifyCompare(pred1: PredicateApp, pred2: PredicateApp): Boolean =
    val (newPred1, newPred2) = PredicateApp.unify(pred1, pred2)
    newPred1 == newPred2

  given Formatter with
    override def emphasize: Format = text => s"_${text}_"

    override def strong: Format = text => s"**$text**"

    override def link(target: String): Format = text => s"[$text]($target)"

    override def itemNumber: String = "1. "

    override def formula: Format = text => s"<span class='formula'>\\\\($text\\\\)</span>"
