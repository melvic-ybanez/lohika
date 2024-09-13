package com.melvic.lohika.prover.interpreters

import cats.implicits.*
import cats.data.WriterT
import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.Cnf.*
import Prover.{Contradiction, Derive, Exhaustion, ResolutionResult}
import com.melvic.lohika.{Clauses, Cnf, Formatter, Equivalence, Parser, Problem}
import com.melvic.lohika.formula.Formula
import fastparse.Parsed
import Formatter.*

object LiveProver:
  import com.melvic.lohika.Givens.given
  import com.melvic.lohika.Clauses.given

  type Steps[X] = WriterT[[R] =>> Either[String, R], List[String], X]

  given liveProver: Prover[Steps] with
    override def splitAllIntoClauses(cnfs: List[Cnf]): Steps[Clauses] =
      if cnfs.isEmpty then WriterT((Nil, Clauses.empty).asRight)
      else step(show"${itemNumber}Split all the clauses from $cnfs", Clauses.fromCnfs(cnfs))

    override def convertAllToCnfs(formulae: List[Formula]): Steps[List[Cnf]] =
      formulae match
        case Nil => subStep("No formulae to convert".emphasize, Nil)
        case _ =>
          val cnfsString = if formulae.size > 1 then "their CNFs" else "its CNF"
          val cnfs = formulae.foldLeft(
            List(show"$indent* Convert $formulae into $cnfsString:"),
            List.empty[Cnf]
          ): (step, formula) =>
            step.flatMap: cnfs =>
              val cnf = Cnf.fromFormula(formula)
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
      def recurse(clauses: Clauses): ResolutionResult = clauses.underlying.toList match
        case Nil => Exhaustion
        case clause :: rest =>
          rest
            .map(clause -> _)
            .collectFirstSome(resolvePair(_, _).flatMap:
              case Derive(_, _, clause) if clauseSet.contains(clause) =>
                None
              case result => Some(result)
            )
            .getOrElse(recurse(Clauses(rest*)))

      step(recurse(clauseSet))

    override def write(description: String): Steps[Unit] =
      step(s"${itemNumber}$description", ())

    override def parseProblem(rawAssumptions: String, rawProposition: String): Steps[Problem] =
      Parser.parseFormulae(rawAssumptions) match
        case Parsed.Success(assumptions, _) =>
          Parser.parseFormula(rawProposition) match
            case Parsed.Success(proposition, _) => step(Problem(assumptions, proposition))
            case Parsed.Failure(label, _, _) =>
              WriterT(s"Unable to parse proposition. Message: $label".asLeft)
        case Parsed.Failure(label, _, _) =>
          WriterT(s"Unable to parse assumptions. Message: $label".asLeft)

  def step[A](description: String, value: A): Steps[A] =
    WriterT((description :: Nil, value).asRight)

  def subStep[A](description: String, value: A): Steps[A] =
    step(s"$indent* $description", value)

  def step[A](value: A): Steps[A] = WriterT((Nil, value).asRight)

  def resolvePair: (Clause, Clause) => Option[Derive | Contradiction] =
    case (lit1: Literal, lit2: Literal) => complementary(lit1, lit2).map(Contradiction(_, _))
    case (lit: Literal, or: COr)        => resolvePair(COr(lit :: Nil), or)
    case (or: COr, lit: Literal)        => resolvePair(or, COr(lit :: Nil))
    case (cor1 @ COr(literals1), cor2 @ COr(literals2)) =>
      literals1
        .collectFirstSome(lit1 => literals2.collectFirstSome(complementary(lit1, _)))
        .map: (lit1, lit2) =>
          val newLiterals1 = literals1.filterNot(_ == lit1)
          val newLiterals2 = literals2.filterNot(_ == lit2)
          val cOrLiterals = newLiterals1 ++ newLiterals2

          if cOrLiterals.isEmpty then Contradiction(lit1, lit2)
          else Derive(cor1, cor2, COr(cOrLiterals))

  def complementary: (Literal, Literal) => Option[(Literal, Literal)] =
    case (c1 @ CVar(p1), c2 @ CNot(CVar(p2))) if p1 == p2 => Some(c1, c2)
    case (c1 @ CNot(CVar(p1)), c2 @ CVar(p2)) if p1 == p2 => Some(c1, c2)
    case _                                                => None

  given Formatter with
    override def emphasize: Format = text => s"_${text}_"

    override def strong: Format = text => s"**$text**"

    override def link(target: String): Format = text => s"[$text]($target)"

    override def itemNumber: String = "1. "

    override def formula: Format = text => s"<span class='formula'>\\\\($text\\\\)</span>"
