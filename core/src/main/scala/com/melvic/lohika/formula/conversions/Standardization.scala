package com.melvic.lohika.formula.conversions

import cats.Endo
import cats.data.State
import cats.implicits.*
import com.melvic.lohika.expression.Expression
import com.melvic.lohika.expression.Expression.{Var, collect}
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

import scala.annotation.{tailrec, targetName}

private[formula] trait Standardization:
  opaque type TakenNames = Set[String]
  opaque type AllFreeVars = Set[Var]
  opaque type AllBoundVars = List[Var]

  type StandardizeM[F] = AllFreeVars ?=> F => State[AllBoundVars, F]

  private[formula] final case class Standardized(raw: Formula)

  def standardizeAll(sns: List[SimplifiedNegations]): List[Standardized] =
    given AllFreeVars = sns.map(sn => freeVars(using Set.empty)(sn.raw)).combineAll
    standardizeAllM(sns.map(_.raw)).runA(Nil).value.map(Standardized(_))

  def standardize(sn: SimplifiedNegations): Standardized =
    standardizeAll(List(sn)).head

  def standardizeAllM: StandardizeM[List[Formula]] =
    _.traverse: fm =>
      State.modify[AllBoundVars](_ ++ boundVars(fm)).flatMap(_ => standardizeM(fm))

  /**
   * Note: Implications and biconditionals are expected to have been eliminated at this point
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
        val taken = (allBoundVars.map(_.name) ++ summon[AllFreeVars].map(_.name))
          .groupMapReduce(identity)(_ => 1)(_ + _)

        val renamingPairs = (x :: xs).flatMap:
          case Var(name) =>
            Option.when(taken(name) > 1):
              RenamingPair(name, generateSymbolName(name, taken.keys.toSet))

        // Decrement the count of renamed vars.
        // Note: we don't use `List.filterNot` or `Set.diff` because we don't want to
        // remove all the intersections. We just want to decrease the corresponding counts.
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

  private def standardizeFListM(fList: FList)(using AllFreeVars): State[AllBoundVars, FList.Args] =
    convertFListM(fList)(standardizeM)

  private[formula] def freeVars(using quantifiedNames: TakenNames): Expression => AllFreeVars =
    Expression.freeVarNames(_).map(Var(_))

  private[formula] def boundVars: Formula => AllBoundVars =
    collect:
      case Quantified(_, (x, xs), matrix) => (x :: xs) ++ boundVars(matrix)

  def generateSymbolName(base: String, taken: TakenNames): String =
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

  object TakenNames:
    def apply(names: Set[String]): TakenNames =
      names

    def empty: TakenNames =
      Set.empty

  extension (freeVars: AllFreeVars) def raw: Set[Var] = freeVars

  extension (takenNames: TakenNames)
    @targetName("names")
    def raw: Set[String] = takenNames

    def modify(f: Endo[Set[String]]): TakenNames =
      f(takenNames)
