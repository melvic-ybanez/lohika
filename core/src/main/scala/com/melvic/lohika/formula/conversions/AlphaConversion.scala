package com.melvic.lohika.formula.conversions

import cats.Endo
import com.melvic.lohika.formula.Formula
import com.melvic.lohika.formula.Formula.*

private[formula] trait AlphaConversion:
  final case class RenamingPair(originalName: String, newName: String)
  type AlphaConvert[F <: Formula] = RenamingPair ?=> Endo[F]

  def originalName(using renamingPair: RenamingPair): String =
    renamingPair.originalName

  def newName(using renamingPair: RenamingPair): String =
    renamingPair.newName

  def alphaConvert: AlphaConvert[Formula] =
    case forall: Forall           => alphaConvertUniversal(forall)
    case thereExists: ThereExists => alphaConvertExistential(thereExists)
    case fm                       => fm

  def alphaConvertQuantified: AlphaConvert[Quantified] =
    case Quantified(quantifier, (Var(name), xs), matrix) if name == originalName =>
      Quantified(quantifier, (Var(newName), xs), renameFreeVars(matrix))
    case quantified @ Quantified(quantifier, (_, Nil), _) => quantified
    case Quantified(quantifier, (x, y :: ys), matrix)     =>
      // rename without the first variable
      alphaConvertQuantified(Quantified(quantifier, (y, ys), matrix)) match
        // put the variable back into the renamed quantified formula
        case Quantified(quantifier, (y1, ys1), matrix1) =>
          Quantified(quantifier, (x, y1 :: ys1), matrix1)

  def alphaConvertUniversal: AlphaConvert[Forall] =
    alphaConvertQuantified andThen:
      case forall: Forall => forall

  def alphaConvertExistential: AlphaConvert[ThereExists] =
    alphaConvertQuantified andThen:
      case thereExists: ThereExists => thereExists

  def renameVariable: AlphaConvert[Var] =
    case Var(name) if name == originalName => Var(newName)
    case variable                          => variable

  def renameFreeVars: AlphaConvert[Formula] =
    case v: Var => renameVariable(v) // we may not need this
    case PredicateApp(name, args) =>
      PredicateApp(
        name,
        args.map {
          case varArg: Var => renameVariable(varArg)
          case fm          => fm
        }
      )
    // the variable is not free, return as-is
    case quantified @ Quantified(_, (Var(x), xs), _)
        if x == originalName || xs.exists(_.name == originalName) =>
      quantified
    case Quantified(quantifier, vars, matrix) =>
      Quantified(quantifier, vars, renameFreeVars(matrix))
    case fm => Formula.convertBy(renameFreeVars)(fm)
