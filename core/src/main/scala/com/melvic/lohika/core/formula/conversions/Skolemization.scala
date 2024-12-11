package com.melvic.lohika.core.formula.conversions

import cats.Endo
import cats.data.State
import cats.implicits.*
import com.melvic.lohika.core.expression.Expression
import Expression.*
import com.melvic.lohika.core.formula.Formula
import Formula.*

private[formula] trait Skolemization:
  private[formula] final case class Snf(raw: Formula)

  opaque type UniversalVars = Set[Var]
  opaque type ExistentialVars = Set[Var]
  type StateData = (TakenNames, UniversalVars)
  type Skolemize[E] = E => State[StateData, E]

  def skolemize(pnf: Pnf): Snf =
    skolemizeAll(List(pnf)).head

  def skolemizeAll(pnfs: List[Pnf]): List[Snf] =
    val functionNames = pnfs.map(pnf => Expression.functionNames(pnf.raw)).combineAll
    skolemizeAllM(pnfs.map(_.raw)).runA(TakenNames(functionNames), Set.empty[Var]).value.map(Snf(_))

  def skolemizeAllM: Skolemize[List[Formula]] =
    _.traverse: fm =>
      State
        .modify[StateData] { case (taken, universalVars) =>
          val freeVarNames = Expression.freeVarNames(using Set.empty)(fm)
          (taken.modify(_ ++ universalVars.map(_.name) ++ freeVarNames), Set.empty)
        }
        .flatMap(_ => skolemizeM(fm))

  def skolemizeM: Skolemize[Formula] =
    case ThereExists((x, xs), matrix) =>
      val boundVars = x :: xs
      State
        .get[StateData]
        .flatMap: (_, universalVars) =>
          val replaceVars =
            if universalVars.nonEmpty then replaceWithSkolemFunctions(using boundVars.toSet)(matrix)
            // If there are no bound variables, then there are no universal quantifiers that appear
            // before this existential quantifier.
            else State.pure(replaceWithSkolemConstants(using boundVars.map(_.name))(matrix))

          for
            replacedVars     <- replaceVars
            skolemizedMatrix <- skolemizeM(replacedVars)
          yield skolemizedMatrix
    case Forall(bounds @ (x, xs), matrix) =>
      for
        _ <- State.modify[StateData]((taken, vars) => (taken, vars ++ (x :: xs).toSet))
        skolemizedMatrix <- skolemizeM(matrix)
      yield Forall(bounds, skolemizedMatrix)
    case fm => State.pure(fm)

  /**
   * We can reuse the names of the bound variables as Skolem constants without worrying about any
   * potential name clashes, since the formula has been standardized such that all first order
   * variables have unique names.
   */
  private def replaceWithSkolemConstants(using constNames: List[String]): Endo[Formula] =
    case PredicateApp(name, args) => PredicateApp(name, args.map(replaceTermWithSkolemConstants))
    case fm                       => convertBy(replaceWithSkolemConstants)(fm)

  private def replaceTermWithSkolemConstants(using constNames: List[String]): Endo[Term] =
    case Var(name) if constNames.contains(name) => Const(name)
    case FunctionApp(name, args) =>
      FunctionApp(name, args.map(replaceTermWithSkolemConstants))
    case term => term

  private def replaceWithSkolemFunctions(using ExistentialVars): Skolemize[Formula] =
    case Quantified(quantifier, boundVars, matrix) =>
      replaceWithSkolemFunctions(matrix).map(Quantified(quantifier, boundVars, _))
    case PredicateApp(name, args) =>
      args.map(replaceTermsWithSkolemFunctions).sequence.map(PredicateApp(name, _))
    case or: Or   => skolemizeFListM(or).map(Or.fromList)
    case and: And => skolemizeFListM(and).map(And.fromList)
    case Not(p)   => replaceWithSkolemFunctions(p).map(Not.apply)
    case fm       => State.pure(fm)

  private def replaceTermsWithSkolemFunctions(using
      existentialVars: ExistentialVars
  ): Skolemize[Term] =
    case v: Var if existentialVars.contains(v) =>
      State:
        case (takenNames, universalVars) =>
          val functionName =
            generateSymbolName("e", takenNames.modify(_ ++ universalVars.map(_.name)))
          (
            (takenNames.modify(_ + functionName), universalVars),
            FunctionApp(functionName, universalVars.toList)
          )
    case fm => State.pure(fm)

  private def skolemizeFListM(fList: FList)(using
      ExistentialVars
  ): State[StateData, List[Formula]] =
    State: stateData =>
      val (updatedTakenNames, skolemizedComponents) = fList.components
        .map(fm => replaceWithSkolemFunctions(fm).run(stateData).value)
        .foldLeft(TakenNames.empty, List.empty[Formula]):
          case ((takenNamesAcc, fms), ((takenNames, _), fm)) =>
            (takenNamesAcc.modify(_ ++ takenNames.raw), fm :: fms)

      ((updatedTakenNames, stateData._2), skolemizedComponents.reverse)
