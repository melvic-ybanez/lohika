package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Formula.*

object AlphaConverter:
  type NameConversionTable = Map[String, String]
  type Convert[F <: Formula] = NameConversionTable ?=> Endo[F]

  def nameConversionTable(using NameConversionTable): NameConversionTable =
    summon[NameConversionTable]

  def convertFormula: Convert[Formula] =
    case forall: Forall           => convertUniversal(forall)
    case thereExists: ThereExists => convertExistential(thereExists)
    case fm =>
      Converter.convertFormula(fm).unless { case _: Forall | _: ThereExists => }.by(convertFormula)

  def convertQuantified: Convert[Quantified] =
    case Quantified(quantifier, (Var(name), xs), matrix) if nameConversionTable.contains(name) =>
      val newName = nameConversionTable(name)
      Quantified(quantifier, (Var(newName), xs), rename(matrix))
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
    case v @ Var(name) => nameConversionTable.get(name).fold(v)(Var.apply)

  /**
   * Unlike alpha-conversion, renaming in Lohika is a more general conversion that applies to all
   * first-order variables within the scope, regardless of whether they are bound by a quantifier
   */
  def rename: Convert[Formula] =
    case v: Var                 => renameVariable(v)
    case Predicate(name, args)  => Predicate(name, args.map(renameVariable))
    case quantified: Quantified => convertQuantified(quantified)
    case fm =>
      Converter
        .convertFormula(fm)
        .unless { case _: Forall | _: ThereExists | _: Var | _: Predicate => }
        .by(rename)
