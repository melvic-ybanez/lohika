package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Formula.*

object AlphaConverter:
  final case class RenamingPair(originalName: String, newName: String)
  type Convert[F <: Formula] = RenamingPair ?=> Endo[F]

  def originalName(using renamingPair: RenamingPair): String =
    renamingPair.originalName

  def newName(using renamingPair: RenamingPair): String =
    renamingPair.newName

  def convertFormula: Convert[Formula] =
    case forall: Forall           => convertUniversal(forall)
    case thereExists: ThereExists => convertExistential(thereExists)
    case fm                       => fm

  def convertQuantified: Convert[Quantified] =
    case Quantified(quantifier, (Var(name), xs), matrix) if name == originalName =>
      Quantified(quantifier, (Var(newName), xs), renameFreeVars(matrix))
    case quantified @ Quantified(quantifier, (_, Nil), _) => quantified
    case Quantified(quantifier, (x, y :: ys), matrix)     =>
      // rename without the first variable
      convertQuantified(Quantified(quantifier, (y, ys), matrix)) match
        // put the variable back into the renamed quantified formula
        case Quantified(quantifier, (y1, ys1), matrix1) =>
          Quantified(quantifier, (x, y1 :: ys1), matrix1)

  def convertUniversal: Convert[Forall] =
    convertQuantified andThen:
      case forall: Forall => forall

  def convertExistential: Convert[ThereExists] =
    convertQuantified andThen:
      case thereExists: ThereExists => thereExists

  def renameVariable: Convert[Var] =
    case Var(name) if name == originalName => Var(newName)
    case variable                          => variable

  def renameFreeVars: Convert[Formula] =
    case v: Var                => renameVariable(v) // we may not need this
    case Predicate(name, args) => Predicate(name, args.map(renameVariable))
    // the variable is not free, return as-is
    case quantified @ Quantified(_, (Var(x), xs), _)
        if x == originalName || xs.exists(_.name == originalName) =>
      quantified
    case Quantified(quantifier, vars, matrix) =>
      Quantified(quantifier, vars, renameFreeVars(matrix))
    case fm =>
      Converter
        .convertFormula(fm)
        .unless { case _: Forall | _: ThereExists | _: Var | _: Predicate => }
        .by(renameFreeVars)
