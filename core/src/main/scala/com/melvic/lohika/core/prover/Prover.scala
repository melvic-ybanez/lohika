package com.melvic.lohika.core.prover

import cats.implicits.toFoldableOps
import com.melvic.lohika.core.Formatter
import com.melvic.lohika.core.Formatter.Format
import com.melvic.lohika.core.formula.Cnf.*
import com.melvic.lohika.core.formula.Formula.PredicateApp
import com.melvic.lohika.core.formula.{Clauses, Cnf, Formula}
import com.melvic.lohika.core.meta.Entailment
import com.melvic.lohika.core.meta.Entailment.Direct
import com.melvic.lohika.core.parsers.Parser
import com.melvic.lohika.core.prover.Proof.*
import com.melvic.lohika.core.prover.Proof.Step.*
import fastparse.Parsed

import scala.annotation.tailrec

object Prover:
  type Result[A] = Either[String, A]

  def prove(rawEntailment: String)(using Formatter): Result[(Entailment, Proof)] =
    parseEntailment(rawEntailment).map: entailment =>
      val Direct(premises, conclusion) = Entailment.unfold(entailment)
      val negatedConclusion = !Formula.addImpliedForall(conclusion)
      val (conversions, cnfs) = convertCnfsToSteps(premises ++ List(negatedConclusion))
      val firstStep = Nullary(ConclusionNeg(negatedConclusion))
      val clauseMap = conversions
        .zip(cnfs)
        .flatMap: (conversion, cnf) =>
          val conversionProof = Unary(conversion, firstStep)
          cnf match
            case clause: Clause => List(clause -> conversionProof)
            case cAnd @ CAnd(clauses) =>
              clauses.toSet.toList.map: clause =>
                clause -> Unary(AndElim(cAnd, clause), conversionProof)
        .toMap
      entailment -> resolveRecursively(clauseMap)(using premises, conclusion)

  @tailrec
  def resolveRecursively(
      clauseMap: ClauseMap
  )(using premises: List[Formula], conclusion: Formula)(using Formatter): Proof =
    resolve(Clauses(clauseMap.keys.toList*)) match
      case Exhaustion => Unary(Conclusion.notProvable, Nullary(Exhaustion))
      case contradiction @ Contradiction(p, q) =>
        Unary(
          Conclusion.provable,
          Binary(contradiction, clauseMap.proofOf(p), clauseMap.proofOf(q))
        )
      case derived @ Derived(left, right, clause) =>
        resolveRecursively(
          clauseMap + (clause -> Binary(derived, clauseMap.proofOf(left), clauseMap.proofOf(right)))
        )

  def parseEntailment(rawEntailment: String): Result[Entailment] =
    Parser.parseEntailment(rawEntailment) match
      case Parsed.Success(entailment, _)   => Right(entailment)
      case Parsed.Failure(label, _, extra) => Left(s"Unable to parse '$rawEntailment'.")

  def convertCnfsToSteps(formulae: List[Formula]): (List[Identity | Rewrite], List[Cnf]) =
    val cnfs = Formula.toCnfAll(formulae)
    val steps = formulae
      .zip(cnfs)
      .map:
        case (fm, cnf) if Formula.isInCnf(fm) => Identity(cnf)
        case (fm, cnf)                        => Conversion(fm, cnf)
    (steps, cnfs)

  def resolve(clauseSet: Clauses): ResolutionResult =
    @tailrec
    def outerLoop(clauses: Clauses, result: ResolutionResult): ResolutionResult =
      clauses.toList match
        case Nil => result
        case clause :: rest =>
          def innerLoop(pairs: List[(Clause, Clause)], result: ResolutionResult): ResolutionResult =
            pairs match
              case Nil => result
              case (left, right) :: rest =>
                resolvePair(left, right).fold(innerLoop(rest, result)):
                  case contradiction: Contradiction => contradiction
                  case Derived(_, _, clause) if clauseSet.contains(clause) =>
                    innerLoop(rest, result)
                  // we continue the search, in case there will be a contradiction ahead
                  case derived => innerLoop(rest, derived)

          innerLoop(rest.map(clause -> _), result) match
            case contradiction: Contradiction => contradiction
            case altResult                    => outerLoop(Clauses(rest*), altResult)

    outerLoop(clauseSet, Exhaustion)

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

          // note that we convert this to set to remove duplicates disjuncts (e.g. A | A)
          val cOrLiterals = (newLiterals1 ++ newLiterals2).toSet

          if cOrLiterals.isEmpty then Contradiction(lit1, lit2)
          else
            val (clause1, clause2) = (cor1, cor2) match
              case (COr(lit1 :: Nil), COr(lit2 :: Nil)) => (lit1, lit2)
              case (COr(lit1 :: Nil), clause2)          => (lit1, clause2)
              case (clause1, COr(lit2 :: Nil))          => (clause1, lit2)
              case _                                    => (cor1, cor2)
            Derived(
              clause1,
              clause2,
              if cOrLiterals.size == 1 then cOrLiterals.head else COr(cOrLiterals.toList)
            )

  def complementary: (CLiteral, CLiteral) => Option[(CLiteral, CLiteral)] =
    case (p1: PredicateApp, c2 @ CNot(p2: PredicateApp)) if unifyCompare(p1, p2) => Some(p1, c2)
    case (c1 @ CNot(p1: PredicateApp), p2: PredicateApp) if unifyCompare(p1, p2) => Some(c1, p2)
    case _                                                                       => None

  def unifyCompare(pred1: PredicateApp, pred2: PredicateApp): Boolean =
    val (newPred1, newPred2) = PredicateApp.unify(pred1, pred2)
    newPred1 == newPred2

  opaque type ClauseMap = Map[Clause, Proof]

  extension (clauseMap: ClauseMap)
    def proofOf(clause: Clause): Proof =
      clauseMap(clause)

  given Formatter with
    override def emphasize: Format = text => s"_${text}_"

    override def strong: Format = text => s"**$text**"

    override def link(target: String): Format = text => s"[$text]($target)"

    override def itemNumber: String = "1. "

    override def formula: Format = text => s"<span class='formula'>\\\\($text\\\\)</span>"

    override def newline: String = "\n\n"

    override def sentence: Format = text => s"<span class='sentence'>$text.</span>"
