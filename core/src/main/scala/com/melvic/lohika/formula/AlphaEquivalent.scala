package com.melvic.lohika.formula

import cats.Endo
import com.melvic.lohika.formula.Converter.convertFormula
import com.melvic.lohika.formula.Formula.*

object AlphaEquivalent:
  type NameMapping = Map[String, String]
  type Rename[F <: Formula] = NameMapping ?=> Endo[F]

  def nameMapping(using NameMapping): NameMapping =
    summon[NameMapping]

  def fromQuantified: Rename[Quantified] =
    case Quantified(quantifier, (Var(name), xs), matrix) if nameMapping.contains(name) =>
      val newName = nameMapping(name)
      Quantified(quantifier, (Var(newName), xs), rename(matrix))
    case quantified @ Quantified(quantifier, (_, Nil), _) => quantified
    case Quantified(quantifier, (x, y :: ys), matrix)     =>
      // rename without the first variable
      fromQuantified(Quantified(quantifier, (y, ys), matrix)) match
        // put the variable back into the renamed quantified formula
        case Quantified(quantifier, (y1, ys1), matrix1) =>
          Quantified(quantifier, (x, y1 :: ys1), matrix1)

  def fromForall: Rename[Forall] =
    fromQuantified andThen:
      case forall: Forall => forall

  def fromThereExists: Rename[ThereExists] =
    fromQuantified andThen:
      case thereExists: ThereExists => thereExists

  def rename: Rename[Formula] =
    case v @ Var(name) => nameMapping.get(name).fold(v)(Var.apply)
    case fm => convertFormula(fm).unless { case _: Forall | _: ThereExists => }.by(rename)
