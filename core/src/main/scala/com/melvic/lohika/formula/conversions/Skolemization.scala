package com.melvic.lohika.formula.conversions

import cats.data.State
import cats.implicits.*
import com.melvic.lohika.formula.{Expression, Formula}
import com.melvic.lohika.formula.Formula.*

private[formula] trait Skolemization:
  private[formula] final case class Snf(raw: Formula)

  opaque type UniversalVars = Set[Var]
  opaque type ExistentialVars = Set[Var]
  type StateData = (TakenNames, UniversalVars)
  type Skolemize[E <: Expression] = E => State[StateData, E]

  def skolemize: Pnf => Snf =
    case Pnf(fm) =>
      def recurse: Skolemize[Formula] =
        case ThereExists((x, xs), matrix) =>
          State
            .get[StateData]
            .flatMap: (_, universalVars) =>
              val replaceVars =
                if universalVars.nonEmpty then
                  replaceWithSkolemFunctions(using (x :: xs).toSet)(matrix)
                // If there are no bound variables, then there are no universal quantifiers that appear
                // before this existential quantifier.
                else replaceWithSkolemConstants(matrix)

              for
                replacedVars     <- replaceVars
                skolemizedMatrix <- recurse(replacedVars)
              yield skolemizedMatrix
        case Forall(bounds @ (x, xs), matrix) =>
          for
            _ <- State.modify[StateData]((taken, vars) => (taken, vars ++ (x :: xs).toSet))
            skolemizedMatrix <- recurse(matrix)
          yield Forall(bounds, skolemizedMatrix)
        case fm => State.pure(fm)

      val takenNames = allFreeVars(using TakenNames.empty)(fm).raw.map(_.name)
      Snf(recurse(fm).runA(TakenNames.fromSet(takenNames), Set.empty[Var]).value)

  /**
   * We can reuse the names of the bound variables as Skolem constants without worrying about any
   * potential name clashes, since the formula has been standardized such that all first order
   * variables have unique names.
   */
  private def replaceWithSkolemConstants: Skolemize[Formula] =
    State.pure

  private def replaceWithSkolemFunctions(using
      existentialVars: ExistentialVars
  ): Skolemize[Formula] =
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
          val functionName = generateSymbolName("e", takenNames)
          (
            (TakenNames.fromSet(takenNames.raw + functionName), universalVars),
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
            (TakenNames.fromSet(takenNamesAcc.raw ++ takenNames.raw), fm :: fms)

      ((updatedTakenNames, stateData._2), skolemizedComponents.reverse)
