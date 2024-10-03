package com.melvic.lohika.formula

import cats.data.State
import cats.implicits.*
import com.melvic.lohika.formula.AlphaConverter.RenamingPair
import com.melvic.lohika.formula.Formula.*

import scala.annotation.tailrec

object Standardizer:
  type TakenNames = List[String]
  type Standardize[F <: Formula] = F => State[TakenNames, F]

  /**
   * Note: Implications and biconditionals are expected to have been eliminated at this point
   */
  def standardize: Standardize[Formula] =
    case quantified @ Quantified(quantifier, (x, xs), matrix) =>
      for
        sq <- standardizeQuantified(quantified)
        sm <- standardize(sq.matrix) // we standardize the matrix for nested quantifiers
      yield Quantified(quantifier, sq.boundVars, sm)
    case or: Or   => standardizeFList(or).map(Or.apply)
    case and: And => standardizeFList(and).map(And.apply)
    case Not(p)   => standardize(p).map(Not.apply)
    case fm       => State.pure(fm)

  def standardizeQuantified: Standardize[Quantified] =
    case quantified @ Quantified(_, (x, xs), _) =>
      State: taken =>
        val renamingPairs = (x :: xs).map:
          case Var(name) =>
            val newName =
              // we explicitly check for non-emptiness (which isn't technically necessary)
              // because we don't want to rename the first variable
              if taken.nonEmpty && taken.contains(name) then generateNewName(name, taken)
              else name
            RenamingPair(name, newName)

        (
          renamingPairs.map(_.newName) ++ taken,
          renamingPairs
            .filterNot(pair => pair.originalName == pair.newName)
            .foldLeft(quantified): (fm, renamingPair) =>
              given RenamingPair = renamingPair

              AlphaConverter.convertQuantified(quantified)
        )

  def standardizeFList(fList: FList): State[TakenNames, (Formula, Formula, List[Formula])] =
    for
      sp  <- standardize(fList.p)
      sq  <- standardize(fList.q)
      srs <- fList.rs.map(standardize).sequence
    yield (sp, sq, srs)

  def generateNewName(base: String, taken: List[String]): String =
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
