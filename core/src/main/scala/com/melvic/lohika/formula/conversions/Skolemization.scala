package com.melvic.lohika.formula.conversions

import cats.data.State
import cats.implicits.*
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

private[formula] trait Skolemization:
  private[formula] final case class Snf(raw: Formula)

  opaque type UniversalVars = Set[Var]
  opaque type ExistentialVars = Set[Var]
  type StateData = (TakenNames, UniversalVars)
  type Skolemize = Formula => State[StateData, Formula]

  def skolemize: Pnf => Snf =
    case Pnf(fm) =>
      def recurse: Skolemize =
        case ThereExists((x, xs), matrix) =>
          State
            .get[StateData]
            .flatMap: (takenNames, boundVars) =>
              val replaceVars =
                if boundVars.nonEmpty then
                  replaceVarsWithSkolemFunctions(using (x :: xs).toSet)(matrix)
                // If there are no bound variables, then there are no universal quantifiers that appear
                // before this existential quantifier.
                else replaceVarsWithSkolemConstants(matrix)

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
      Snf(recurse(fm).run(TakenNames.fromSet(takenNames), Set.empty[Var]).value._2)

  /**
   * We can reuse the names of the bound variables as Skolem constants without worrying about any
   * potential name clashes, since the formula has been standardized such that all first order
   * variables have unique names.
   */
  private def replaceVarsWithSkolemConstants: Skolemize =
    State.pure

  private def replaceVarsWithSkolemFunctions(using existentialVars: ExistentialVars): Skolemize =
    case ThereExists((x, xs), matrix) =>
      replaceVarsWithSkolemFunctions(using existentialVars ++ (x :: xs))(matrix)
    case v: Var if existentialVars.contains(v) =>
      State:
        case (takenNames, universalVars) =>
          val functionName = generateSymbolName("e", takenNames)
          (
            (TakenNames.fromSet(takenNames.raw + functionName), universalVars),
            FunctionApp(functionName, universalVars.toList)
          )
    case Predicate(name, args) =>
      val skolemizedArgs = args.foldLeft(State.pure[StateData, List[Term]](Nil)): (acc, arg) =>
        replaceVarsWithSkolemFunctions(arg).flatMap:
          case skolemizedArg: Term => acc.map(acc => skolemizedArg :: acc)
          case _                   => acc // probably unreachable

      skolemizedArgs.map(args => Predicate(name, args.reverse))
    case or: Or   => skolemizeFListM(or).map(Or.apply)
    case and: And => skolemizeFListM(and).map(And.apply)
    case Not(p)   => replaceVarsWithSkolemFunctions(p).map(Not.apply)
    case fm       => State.pure(fm)

  private def skolemizeFListM(fList: FList)(using ExistentialVars): State[StateData, FList.Args] =
    convertFListM(fList)(replaceVarsWithSkolemFunctions)
