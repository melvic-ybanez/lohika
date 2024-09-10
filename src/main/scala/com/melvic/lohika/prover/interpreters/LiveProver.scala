package com.melvic.lohika.prover.interpreters

import cats.implicits.*
import cats.data.Writer
import com.melvic.lohika.Cnf.*
import com.melvic.lohika.{Clauses, Cnf, Equivalence}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.prover.algebras.Prover
import com.melvic.lohika.prover.algebras.Prover.{
  Contradiction,
  Exhaustion,
  NewClause,
  ResolutionResult
}

object LiveProver:
  import com.melvic.lohika.Givens.given

  type Step[X] = Writer[List[String], X]

  given liveProver: Prover[Step] with
    override def splitAllIntoClauses(cnfs: List[Cnf]): Step[Clauses] =
      step(s"Split all the clauses from ${cnfs.show}.", Clauses.fromCnfs(cnfs))

    override def convertAllToCnfs(formulae: List[Formula]): Step[List[Cnf]] =
      formulae.foldLeft(Writer(List(s"Convert ${formulae.show} to CNFs"), List.empty[Cnf])):
        (step, formula) =>
          step.flatMap: cnfs =>
            val cnf = Cnf.fromFormula(formula)
            Writer("* " + Equivalence(formula, cnf).show :: Nil, cnf :: cnfs)

    override def updateClauseSet(clauseSet: Clauses, newClauses: Clauses): Step[Clauses] =
      val newClauseSet = clauseSet ++ newClauses
      step(
        s"Add ${newClauses.show} to the clause set. Here's the updated clause set: ${newClauseSet.show}",
        newClauseSet
      )

    override def transform(lhs: Formula, rhs: Formula): Step[Formula] =
      step(s"${lhs.show} becomes ${rhs.show}", rhs)

    override def resolve(clauseSet: Clauses): Step[ResolutionResult] =
      def recurse(clauses: Clauses): ResolutionResult = clauses.underlying.toList match
        case Nil => Exhaustion
        case clause :: rest =>
          rest
            .map(clause -> _)
            .collectFirstSome(resolvePair(_, _))
            .getOrElse(recurse(Clauses(rest*)))

      step(recurse(clauseSet))

    override def describe(description: String): Step[Unit] =
      step(description, ())

  def step[A](description: String, value: A): Step[A] =
    Writer(description :: Nil, value)

  def step[A](value: A): Step[A] = Writer(Nil, value)

  def resolvePair: (Clause, Clause) => Option[NewClause | Contradiction] =
    case (lit1: Literal, lit2: Literal) => complementary(lit1, lit2).map(Contradiction(_, _))
    case (lit: Literal, or: COr)        => resolvePair(COr(lit :: Nil), or)
    case (or: COr, lit: Literal)        => resolvePair(or, COr(lit :: Nil))
    case (COr(literals1), COr(literals2)) =>
      literals1
        .collectFirstSome(lit1 => literals2.collectFirstSome(complementary(lit1, _)))
        .map: (lit1, lit2) =>
          val newLiterals1 = literals1.filterNot(_ == lit1)
          val newLiterals2 = literals2.filterNot(_ == lit2)
          NewClause(COr(newLiterals1 ++ newLiterals2))

  def complementary: (Literal, Literal) => Option[(Literal, Literal)] =
    case (c1 @ CVar(p1), c2 @ CNot(CVar(p2))) if p1 == p2 => Some(c1, c2)
    case (c1 @ CNot(CVar(p1)), c2 @ CVar(p2)) if p1 == p2 => Some(c1, c2)
    case _                                                => None
