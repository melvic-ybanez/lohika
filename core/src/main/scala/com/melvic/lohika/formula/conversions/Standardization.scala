package com.melvic.lohika.formula.conversions

import cats.data.State
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

import scala.annotation.tailrec

private[formula] trait Standardization:
  opaque type TakenNames = Set[String]
  opaque type AllFreeVars = Set[Var]
  opaque type AllBoundVars = List[Var]

  type StandardizeM[F <: Formula] = AllFreeVars ?=> F => State[AllBoundVars, F]

  def standardize(noConditionals: NoConditionals): Formula =
    given AllFreeVars = allFreeVars(using Set.empty)(noConditionals.unwrap)
    val boundVars = allBoundVars(noConditionals.unwrap)
    standardizeM(noConditionals.unwrap).run(boundVars).value._2

  /**
   * Note: Implications and biconditionals are expected to have been eliminated at this point, as
   * captured by [[standardize]]'s function signature
   */
  private def standardizeM: StandardizeM[Formula] =
    case quantified @ Quantified(quantifier, (x, xs), matrix) =>
      for
        sq <- standardizeQuantifiedM(quantified)
        sm <- standardizeM(sq.matrix) // we standardize the matrix for nested quantifiers
      yield Quantified(quantifier, sq.boundVars, sm)
    case or: Or   => standardizeFListM(or).map(Or.apply)
    case and: And => standardizeFListM(and).map(And.apply)
    case Not(p)   => standardizeM(p).map(Not.apply)
    case fm       => State.pure(fm)

  private def standardizeQuantifiedM: StandardizeM[Quantified] =
    case quantified @ Quantified(_, (x, xs), _) =>
      State: allBoundVars =>
        val takenOrFree = (allBoundVars.map(_.name) ++ summon[AllFreeVars].map(_.name))
          .groupMapReduce(identity)(_ => 1)(_ + _)

        val renamingPairs = (x :: xs).flatMap:
          case Var(name) =>
            Option.when(takenOrFree(name) > 1):
              RenamingPair(name, generateNewName(name, takenOrFree.keys.toSet))

        // decrement the count of renamed vars
        val reducedBoundVars = renamingPairs.foldLeft(allBoundVars): (boundVars, pair) =>
          val i = boundVars.indexOf(Var(pair.originalName))
          if i == -1 then boundVars else boundVars.patch(i, Nil, 1)

        (
          renamingPairs.map(pair => Var(pair.newName)) ++ reducedBoundVars,
          renamingPairs
            .foldLeft(quantified): (fm, renamingPair) =>
              given RenamingPair = renamingPair
              Formula.alphaConvertQuantified(fm)
        )

  private def standardizeFListM(
      fList: FList
  )(using AllFreeVars): State[AllBoundVars, (Formula, Formula, List[Formula])] =
    for
      sp  <- standardizeM(fList.p)
      sq  <- standardizeM(fList.q)
      srs <- fList.rs.map(standardizeM).sequence
    yield (sp, sq, srs)

  private def allFreeVars(using enclosing: TakenNames): Formula => AllFreeVars =
    case fList: FList =>
      allFreeVars(fList.p) ++ allFreeVars(fList.q) ++ fList.rs.map(allFreeVars).combineAll
    case Imply(p, q)                      => allFreeVars(p) ++ allFreeVars(q)
    case Not(p)                           => allFreeVars(p)
    case Var(x) if !enclosing.contains(x) => Set(Var(x))
    case Predicate(_, args)               => args.map(allFreeVars).combineAll
    case Quantified(_, (Var(x), xs), matrix) =>
      allFreeVars(using (x :: xs.map(_.name)).toSet ++ enclosing)(matrix)
    case fm => Set.empty

  private def allBoundVars: Formula => AllBoundVars =
    case fList: FList =>
      allBoundVars(fList.p) ++ allBoundVars(fList.q) ++ fList.rs.map(allBoundVars).combineAll
    case Imply(p, q)                    => allBoundVars(p) ++ allBoundVars(q)
    case Not(p)                         => allBoundVars(p)
    case Var(x)                         => Nil
    case Predicate(_, args)             => Nil
    case Quantified(_, (x, xs), matrix) => (x :: xs) ++ allBoundVars(matrix)
    case fm                             => Nil

  def generateNewName(base: String, taken: TakenNames): String =
    @tailrec
    def findNewCharLastLetter(suggested: Char, visitedZ: Boolean): Option[Char] =
      if suggested == 'z' && visitedZ then None // second z-visit
      else if taken.contains(suggested.toString) then
        val firstZVisit = suggested == 'z'
        val newSuggestion = if firstZVisit then 'a' else (suggested.toInt + 1).toChar
        findNewCharLastLetter(newSuggestion, firstZVisit)
      else Some(suggested)

    findNewCharLastLetter((base.last.toInt + 1).toChar, false)
      .map(base.init + _)
      .getOrElse:
        val lastDigit = base.reverse.takeWhile(_.isDigit).reverse
        if lastDigit.isEmpty then base + "1"
        else base.take(base.length - lastDigit.length) + (lastDigit.toInt + 1)
